#!/usr/bin/env python3
# ==============================================================================
# WRFOUT -> CSV (pelo wrf-python)
# Extrai variáveis no ponto mais próximo de uma latitude/longitude e gera CSV
# ==============================================================================

import argparse
import glob
import os
import sys

import numpy as np
import pandas as pd
from netCDF4 import Dataset
from wrf import ALL_TIMES, getvar

# Variáveis de Xiang et al. (2025)
TARGET_MAP = {
	'Dew point temperature (HTGL)': 'td',
	'Geopotential height (0DEG)': None,
	'Geopotential height (CEIL)': None,
	'Geopotential height (HTFL)': None,
	'Geopotential height (SFC)': 'HGT',
	'High level cloud cover': 'cloud_high',
	'Low level cloud cover': 'cloud_low',
	'Mean sea level pressure (ETA model)': 'slp',
	'Mid level cloud cover': 'cloud_mid',
	'Parcel lifted index (to 500 hPa)': None,
	'Precipitable water': 'pw',
	'Pressure': 'pressure',
	'Probability of precipitation': None,
	'Relative humidity (HTGL)': 'rh',
	'Relative humidity (HYBL)': 'rh',
	'Specific humidity (HTGL)': None,
	'Specific humidity (HYBL)': None,
	'Specific humidity (SPDY)': None,
	'Surface lifted index': None,
	'Surface roughness': None,
	'Temperature (HTGL)': 'T2',
	'Temperature (SFC)': 'TSK',
	'Temperature - sea Temperature': 'SST',
	'Total cloud cover': 'cloud_total',
	'Total column-integrated cloud water': None,
	'Water temperature': 'SSTSK',
	'u-component of wind (HTGL)': 'U10',
	'u-component of wind (SPDY)': 'ua',
	'v-component of wind (HTGL)': 'V10',
	'v-component of wind (SPDY)': 'va',
	'wind_angle': 'wind_dir',
	'wind_speed': 'wind_speed',
}


def ponto_grade(nc, lat, lon):
	xlat = nc.variables['XLAT'][0]
	xlong = nc.variables['XLONG'][0]
	d = (xlat - lat) ** 2 + (xlong - lon) ** 2
	return np.unravel_index(np.argmin(d), d.shape)


def tempos(nc):
	xt = nc.variables['XTIME'][:]
	origin = nc.variables['XTIME'].units.split('since')[-1].strip()
	return pd.to_datetime(xt, unit='m', origin=origin, utc=True)


def normalize_to_time(v, ntime):
	a = np.asarray(v)

	if a.ndim == 0:
		return np.repeat(float(a), ntime)

	if a.ndim == 1:
		if a.shape[0] == ntime:
			return a
		if ntime == 1 and a.shape[0] > 0:
			return np.array([a[0]])
		raise ValueError(f'Incompatibilidade de tamanho: len(valor)={a.shape[0]} vs ntime={ntime}')

	raise ValueError(f'Esperado valor escalar/1D apos extracao no ponto, recebido shape={a.shape}')


def serie(field, iy, ix, ntime):
	a = np.asarray(field)
	if a.ndim == 2:
		return np.repeat(float(a[iy, ix]), ntime)
	if a.ndim == 3:
		s = a[:, iy, ix]
		return normalize_to_time(s, ntime)
	if a.ndim == 4:
		# Prefer [time, level, y, x]. If time is on axis 1, handle that too.
		if a.shape[0] == ntime:
			s = a[:, 0, iy, ix]
			return normalize_to_time(s, ntime)
		if a.shape[1] == ntime:
			s = a[0, :, iy, ix]
			return normalize_to_time(s, ntime)
		if ntime == 1:
			return np.array([a[0, 0, iy, ix]])
		raise ValueError(f'Nao foi possivel identificar eixo de tempo em shape={a.shape} para ntime={ntime}')
	raise ValueError(a.shape)


def extract(nc, key, iy, ix, ntime):
	if key == 'wind_speed':
		f = getvar(nc, 'uvmet10_wspd_wdir', timeidx=ALL_TIMES, meta=False)
		a = np.asarray(f)
		if a.ndim == 4:
			return normalize_to_time(a[0, :, iy, ix], ntime)
		if a.ndim == 3:
			return np.repeat(float(a[0, iy, ix]), ntime)
		raise ValueError(f'Shape inesperado para array de vento: {a.shape}')

	if key == 'wind_dir':
		f = getvar(nc, 'uvmet10_wspd_wdir', timeidx=ALL_TIMES, meta=False)
		a = np.asarray(f)
		if a.ndim == 4:
			return normalize_to_time(a[1, :, iy, ix], ntime)
		if a.ndim == 3:
			return np.repeat(float(a[1, iy, ix]), ntime)
		raise ValueError(f'Shape inesperado para array de vento: {a.shape}')

	if key.startswith('cloud_'):
		f = getvar(nc, 'cloudfrac', timeidx=ALL_TIMES, meta=False)
		a = np.asarray(f)
		if a.ndim == 4:
			if key == 'cloud_low':
				return normalize_to_time(a[0, :, iy, ix], ntime)
			if key == 'cloud_mid':
				return normalize_to_time(a[1, :, iy, ix], ntime)
			if key == 'cloud_high':
				return normalize_to_time(a[2, :, iy, ix], ntime)
			if key == 'cloud_total':
				return normalize_to_time(np.max(a[:, :, iy, ix], axis=0), ntime)

		if a.ndim == 3:
			if key == 'cloud_low':
				return np.repeat(float(a[0, iy, ix]), ntime)
			if key == 'cloud_mid':
				return np.repeat(float(a[1, iy, ix]), ntime)
			if key == 'cloud_high':
				return np.repeat(float(a[2, iy, ix]), ntime)
			if key == 'cloud_total':
				return np.repeat(float(np.max(a[:, iy, ix])), ntime)

		raise ValueError(f'Shape inesperado para array de nuvens: {a.shape}')

	f = getvar(nc, key, timeidx=ALL_TIMES, meta=False)
	return serie(f, iy, ix, ntime)


def processar_diretorio_wrfout(
	wrf_dir,
	lat,
	lon,
	arquivo_targets,
	arq_saida=None,
	pattern='wrfout*',
	dom=None,
	verbose=False,
	quiet_unsupported=False,
):
	"""
	Processa arquivos wrfout de um diretório, extraindo variáveis para (lat, lon).
	Retorna um DataFrame e opcionalmente salva em CSV.
	"""
	with open(arquivo_targets, encoding='utf-8') as f:
		targets = [x.strip() for x in f if x.strip()]

	rows = []
	arquivos = sorted(glob.glob(os.path.join(wrf_dir, pattern)))
	if dom is not None:
		dom_str = f"d{int(dom):02d}"
		prefixo_dom = f"wrfout_{dom_str}"
		arquivos = [fp for fp in arquivos if os.path.basename(fp).startswith(prefixo_dom)]

	files_processed = 0
	extracted_count = 0
	unsupported_count = 0
	failed_count = 0
	for fp in arquivos:
		files_processed += 1
		with Dataset(fp) as nc:
			iy, ix = ponto_grade(nc, lat, lon)
			ts = tempos(nc)
			data = {'datetime': ts}

			for t in targets:
				mapped = TARGET_MAP.get(t)
				if mapped is None:
					data[t] = np.nan
					unsupported_count += 1
					if verbose and not quiet_unsupported:
						print(
							f"[AVISO] Alvo nao suportado '{t}' em {fp}; preenchendo com NaN.",
							file=sys.stderr,
						)
					continue

				try:
					data[t] = extract(nc, mapped, iy, ix, len(ts))
					extracted_count += 1
				except Exception as e:
					# Keep pipeline running when one field is unavailable in a file.
					data[t] = np.nan
					failed_count += 1
					if verbose:
						print(
							(
								f"[AVISO] Falha no alvo '{t}' mapeado para '{mapped}' "
								f"em {fp}; preenchendo com NaN. Erro: {e}"
							),
							file=sys.stderr,
						)

		rows.append(pd.DataFrame(data))

	if not rows:
		if verbose:
			print(f"[AVISO] Nenhum arquivo WRFOUT encontrado em: {wrf_dir}", file=sys.stderr)
		return pd.DataFrame()

	df = pd.concat(rows, ignore_index=True)

	if arq_saida:
		os.makedirs(os.path.dirname(os.path.abspath(arq_saida)), exist_ok=True)
		df.to_csv(arq_saida, index=False)

	if verbose:
		total_slots = files_processed * len(targets)
		print(
			(
				'[RESUMO] '
				f'arquivos={files_processed}, alvos_por_arquivo={len(targets)}, '
				f'total={total_slots}, extraidos={extracted_count}, '
				f'nao_suportados={unsupported_count}, falhas={failed_count}'
			),
			file=sys.stderr,
		)

	return df


def main():
	p = argparse.ArgumentParser(
		description='Extrai variaveis de arquivos WRFOUT para CSV em um ponto (lat/lon).'
	)
	p.add_argument('wrf_dir', help='Diretorio contendo arquivos WRFOUT.')
	p.add_argument('--padrao', '--pattern', dest='pattern', default='wrfout*', help='Padrao de arquivos WRFOUT.')
	p.add_argument('--dom', type=int, default=None, help='Dominio numerico a filtrar (ex.: 1, 2, 3, 4).')
	p.add_argument('--lat', type=float, required=True, help='Latitude do ponto alvo.')
	p.add_argument('--lon', '--long', dest='lon', type=float, required=True, help='Longitude do ponto alvo.')
	p.add_argument('--alvos', '--targets', dest='targets', required=True, help='Arquivo txt com variaveis alvo (uma por linha).')
	p.add_argument('--saida', '--output', dest='output', required=True, help='Caminho do CSV de saida.')
	p.add_argument(
		'--verboso', '--verbose',
		dest='verbose',
		action='store_true',
		help='Exibe diagnosticos de alvos nao suportados ou que falharam.',
	)
	p.add_argument(
		'--silenciar-nao-suportados', '--quiet-unsupported',
		dest='quiet_unsupported',
		action='store_true',
		help='Oculta avisos de alvos propositalmente nao suportados no TARGET_MAP.',
	)

	a = p.parse_args()

	df = processar_diretorio_wrfout(
		wrf_dir=a.wrf_dir,
		lat=a.lat,
		lon=a.lon,
		arquivo_targets=a.targets,
		arq_saida=a.output,
		pattern=a.pattern,
		dom=a.dom,
		verbose=a.verbose,
		quiet_unsupported=a.quiet_unsupported,
	)

	if df.empty:
		print('Nenhum dado extraido.')
	else:
		print(df.head())


if __name__ == '__main__':
	main()
