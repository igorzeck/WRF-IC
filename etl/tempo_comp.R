# Comparação de tempos de execução do modelo WRF por tempo
# Testado com/em: 
# 1. Dados do GFS (preferencialmente 1ºX1º)
# 2. Modelo rodado com 6 cores da CPU para o WRF
# 3. Computador com as seguintes especificações:
# [code]
# System:
#   Kernel: 6.17.0-19-generic arch: x86_64 bits: 64 compiler: gcc v: 13.3.0 clocksource: tsc
# Desktop: Xfce v: 4.18.1 tk: Gtk v: 3.24.41 wm: xfwm4 v: 4.18.0 with: xfce4-panel
# tools: light-locker vt: 7 dm: LightDM v: 1.30.0 Distro: Linux Mint 22.3 Zena
# base: Ubuntu 24.04 noble
# Machine:
#   Type: Desktop System: LENOVO product: 11JAS0BR00 v: ThinkCentre M75s Gen 2
# serial: <superuser required> Chassis: type: 3 serial: <superuser required>
#   Mobo: LENOVO model: 318E v: SDK0L22692 WIN 3792011958478 serial: <superuser required>
#   part-nu: LENOVO_MT_11JA_BU_Think_FM_ThinkCentre M75s Gen 2 uuid: <superuser required>
#   UEFI: LENOVO v: M3AKT3BA date: 06/21/2021
# CPU:
#   Info: 6-core model: AMD Ryzen 5 PRO 4650G with Radeon Graphics bits: 64 type: MT MCP smt: enabled
# arch: Zen 2 rev: 1 cache: L1: 384 KiB L2: 3 MiB L3: 8 MiB
# Speed (MHz): avg: 3590 high: 4267 min/max: 406/4309 boost: enabled cores: 1: 4267 2: 2383
# 3: 2383 4: 3836 5: 4267 6: 3771 7: 4267 8: 2843 9: 4254 10: 2851 11: 4256 12: 3711
# bogomips: 88634
# Flags: avx avx2 ht lm nx pae sse sse2 sse3 sse4_1 sse4_2 sse4a ssse3 svm
# Graphics:
#   Device-1: AMD Renoir [Radeon RX Vega 6 ] vendor: Lenovo driver: amdgpu v: kernel arch: GCN-5
# pcie: speed: 8 GT/s lanes: 16 ports: active: HDMI-A-1 empty: DP-1,DP-2,DP-3 bus-ID: 0b:00.0
# chip-ID: 1002:1636 class-ID: 0300 temp: 34.0 C
# Display: x11 server: X.Org v: 21.1.11 with: Xwayland v: 23.2.6 compositor: xfwm4 v: 4.18.0
# driver: X: loaded: amdgpu unloaded: fbdev,modesetting,vesa dri: radeonsi gpu: amdgpu
# display-ID: :0.0 screens: 1
# Screen-1: 0 s-res: 1280x720 s-dpi: 96 s-size: 339x191mm (13.35x7.52") s-diag: 389mm (15.32")
# Monitor-1: HDMI-A-1 mapped: HDMI-A-0 model: Lenovo LEN S22e-18 serial: <filter> res: 1280x720
# hz: 60 dpi: 68 size: 476x268mm (18.74x10.55") diag: 546mm (21.5") modes: max: 1920x1080
# min: 720x400
# API: EGL v: 1.5 hw: drv: amd radeonsi platforms: device: 0 drv: radeonsi device: 1 drv: swrast
# gbm: drv: kms_swrast surfaceless: drv: radeonsi x11: drv: radeonsi inactive: wayland
# API: OpenGL v: 4.6 compat-v: 4.5 vendor: amd mesa v: 25.2.8-0ubuntu0.24.04.1 glx-v: 1.4
# direct-render: yes renderer: AMD Radeon Graphics (radeonsi renoir ACO DRM 3.64 6.17.0-19-generic)
# device-ID: 1002:1636
# API: Vulkan v: 1.3.275 layers: 3 surfaces: xcb,xlib device: 0 type: integrated-gpu driver: N/A
# device-ID: 1002:1636 device: 1 type: cpu driver: N/A device-ID: 10005:0000
# Audio:
#   Device-1: AMD Renoir Radeon High Definition Audio vendor: Lenovo driver: snd_hda_intel v: kernel
# pcie: speed: 8 GT/s lanes: 16 bus-ID: 0b:00.1 chip-ID: 1002:1637 class-ID: 0403
# Device-2: AMD ACP/ACP3X/ACP6x Audio Coprocessor vendor: Lenovo driver: N/A pcie: speed: 8 GT/s
# lanes: 16 bus-ID: 0b:00.5 chip-ID: 1022:15e2 class-ID: 0480
# Device-3: AMD Family 17h/19h HD Audio vendor: Lenovo driver: snd_hda_intel v: kernel pcie:
#   speed: 8 GT/s lanes: 16 bus-ID: 0b:00.6 chip-ID: 1022:15e3 class-ID: 0403
# API: ALSA v: k6.17.0-19-generic status: kernel-api
# Server-1: PipeWire v: 1.0.5 status: active with: 1: pipewire-pulse status: active
# 2: wireplumber status: active 3: pipewire-alsa type: plugin
# Network:
#   Device-1: Realtek RTL8111/8168/8211/8411 PCI Express Gigabit Ethernet
# vendor: Lenovo RTL8111/8168/8411 driver: r8169 v: kernel pcie: speed: 2.5 GT/s lanes: 1
# port: f000 bus-ID: 08:00.0 chip-ID: 10ec:8168 class-ID: 0200
# IF: eno1 state: up speed: 1000 Mbps duplex: full mac: <filter>
#   Drives:
#   Local Storage: total: 476.94 GiB used: 250.1 GiB (52.4%)
# ID-1: /dev/nvme0n1 vendor: Samsung model: MZVLB512HBJQ-000L7 size: 476.94 GiB speed: 31.6 Gb/s
# lanes: 4 tech: SSD serial: <filter> fw-rev: 5M2QEXF7 temp: 30.9 C scheme: GPT
# Partition:
#   ID-1: / size: 467.89 GiB used: 250.09 GiB (53.5%) fs: ext4 dev: /dev/nvme0n1p2
# ID-2: /boot/efi size: 511 MiB used: 6.1 MiB (1.2%) fs: vfat dev: /dev/nvme0n1p1
# Swap:
#   ID-1: swap-1 type: file size: 2 GiB used: 1.54 GiB (77.1%) priority: -2 file: /swapfile
# USB:
#   Hub-1: 1-0:1 info: hi-speed hub with single TT ports: 10 rev: 2.0 speed: 480 Mb/s lanes: 1
# chip-ID: 1d6b:0002 class-ID: 0900
# Hub-2: 2-0:1 info: super-speed hub ports: 4 rev: 3.1 speed: 10 Gb/s lanes: 1 chip-ID: 1d6b:0003
# class-ID: 0900
# Hub-3: 3-0:1 info: hi-speed hub with single TT ports: 4 rev: 2.0 speed: 480 Mb/s lanes: 1
# chip-ID: 1d6b:0002 class-ID: 0900
# Hub-4: 4-0:1 info: super-speed hub ports: 2 rev: 3.1 speed: 10 Gb/s lanes: 1 chip-ID: 1d6b:0003
# class-ID: 0900
# Hub-5: 5-0:1 info: hi-speed hub with single TT ports: 4 rev: 2.0 speed: 480 Mb/s lanes: 1
# chip-ID: 1d6b:0002 class-ID: 0900
# Hub-6: 6-0:1 info: super-speed hub ports: 2 rev: 3.1 speed: 10 Gb/s lanes: 1 chip-ID: 1d6b:0003
# class-ID: 0900
# Sensors:
#   System Temperatures: cpu: 46.0 C mobo: N/A gpu: amdgpu temp: 34.0 C
# Fan Speeds (rpm): N/A
# Repos:
#   Packages: 2866 pm: dpkg pkgs: 2861 pm: flatpak pkgs: 5
# No active apt repos in: /etc/apt/sources.list
# Active apt repos in: /etc/apt/sources.list.d/antigravity.list
# 1: deb [signed-by=/etc/apt/keyrings/antigravity-repo-key.gpg] https: //us-central1-apt.pkg.dev/projects/antigravity-auto-updater-dev/ antigravity-debian main
# Active apt repos in: /etc/apt/sources.list.d/official-package-repositories.list
# 1: deb https: //mint-packages.c3sl.ufpr.br zena main upstream import backport
# 2: deb http: //sft.if.usp.br/ubuntu noble main restricted universe multiverse
# 3: deb http: //sft.if.usp.br/ubuntu noble-updates main restricted universe multiverse
# 4: deb http: //sft.if.usp.br/ubuntu noble-backports main restricted universe multiverse
# 5: deb http: //security.ubuntu.com/ubuntu/ noble-security main restricted universe multiverse
# Active apt repos in: /etc/apt/sources.list.d/vscode.sources
# 1: deb [arch=amd64] https: //packages.microsoft.com/repos/code stable main
# Info:
#   Memory: total: 16 GiB note: est. available: 14.99 GiB used: 3.11 GiB (20.7%)
# Processes: 367 Power: uptime: 8d 12h 2m states: freeze,mem,disk suspend: deep wakeups: 0
# hibernate: platform Init: systemd v: 255 target: graphical (5) default: graphical
# Compilers: gcc: 13.3.0 Client: Unknown python3.12 client inxi: 3.3.34
# [/code]
# 4. WRF na versão: 
# V4.7.1
# git commit f15568ccc1447780e3bd664b9f0196edd784bf33
# 5. WPS na versão:
# Version 4.6.0
# 6. É possível que as bibliotecas utilizadas para compilar o WRF possam
# Afetar também o desempenho!
# 7. interval_seconds como sendo 3h a 3hd
# Setup ----
library(dplyr)
library(lubridate)
library(ggplot2)

# Arquivo
arq <- "etl/tempo_exec.md"
linhas <- readLines(arq)

cour_ <- 0
t_u <- 0
t_g <- 0
t_m <- 0
t_r <- 0
t_w <- 0

df_t <- tribble(
  ~cour, ~ungrib, ~geogrid, ~metgrid, ~real, ~wrf, ~input_s
)

for (linha in linhas) {
  if (grepl("Cour", linha)) {
    cour_ <- cour_ + 1
  }
  if (grepl("Ungrib", linha)) {
    t_u <- period_to_seconds(ms(unlist(strsplit(linha,":"))[2]))
  }
  if (grepl("Geogrid", linha)) {
    t_g <- period_to_seconds(ms(unlist(strsplit(linha,":"))[2]))
  }
  if (grepl("Metgrid", linha)) {
    t_m <- period_to_seconds(ms(unlist(strsplit(linha,":"))[2]))
  }
  if (grepl("real.exe", linha)) {
    t_r <- period_to_seconds(ms(unlist(strsplit(linha,":"))[2]))
  }
  if (grepl("wrf.exe", linha)) {
    t_w <- period_to_seconds(ms(unlist(strsplit(linha,":"))[2]))
    
    df_t <- df_t %>% 
      add_row(
        cour = cour_,
        ungrib = t_u,
        geogrid = t_g,
        metgrid = t_m,
        real = t_r,
        wrf = t_w,
        input_s = 6 * cour_ * 3600
      )
  }
}

df_t
# Análise ----
df_t %>% 
  ggplot(aes(input_s, wrf)) +
  geom_line()

## Cour Extra (5) ----
# ## Cour 5:
# #### `Ungrib.exe`
# real	2m45,728s
# user	2m19,958s
# sys	0m18,175s
# #### `Geogrid.exe`
# real	0m6,606s
# user	0m5,701s
# sys	0m0,731s
# #### `Metgrid.exe`
# real	13m51,651s
# user	13m23,625s
# sys	0m20,432s
# #### **`mpirun -np 1 ./real.exe`**
# real	4m6,586s
# user	3m53,106s
# sys	0m4,714s
# #### **`mpirun -np 6 ./wrf.exe`**
# real	3551m3,528s
# user	20812m26,081s
# sys	491m13,292s
df_t <- df_t %>% 
  add_row(
    cour = 5,
    ungrib = period_to_seconds(ms("2m45,728s")),
    geogrid = period_to_seconds(ms("0m6,606s")),
    metgrid = period_to_seconds(ms("13m51,651s")),
    real = period_to_seconds(ms("4m6,586s")),
    wrf = period_to_seconds(ms("3551m3,528s")),
    input_s = 31 * 24 * 3600
  )

## Cria valores para a previsão de tempo com regressão linear ----
modelo = df_t[1:4,] %>%
  lm(data = ., formula = wrf ~ input_s)
modelo
summary(modelo)
# (Intercept)      input_s  
# 48.0120       0.0776 
# t_previsto(t_input) = 0.0776 * t_input + 48.0120
# Comparando com os valores do Cour: 5
df_t[5,]
valor_previsto <- predict(modelo, newdata = df_t[5,])
print(seconds_to_period(valor_previsto))
print(seconds_to_period(df_t[5,]$wrf))
paste0(round((df_t[5,]$wrf - valor_previsto) / df_t[5,]$wrf * 100, 2),"%")
# Erro relativo de 2.43%
# Isso mesmo com todas as diferenças de input (mais domínios, além de área diferente!)
saveRDS(modelo, "datasets/modelos/modelo_lm.rds")
