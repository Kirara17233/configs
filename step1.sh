#!/usr/bin/zsh

# 更新系统时间
timedatectl set-ntp true

# 分区与格式化

echo "g
n


+512M
t
1
n



w" | fdisk /dev/sda

# 格式化分区
mkfs.fat -F32 /dev/sda1
mkfs.ext4 /dev/sda2

# 挂载分区
mount /dev/sda2 /mnt
mkdir /mnt/boot
mount /dev/sda1 /mnt/boot

# 安装基本包
pacstrap /mnt base base-devel linux linux-firmware dhcpcd openssh neovim sudo zsh git

# 配置shell
rm /mnt/etc/skel/.bash*
sed -i "s|/bin/bash|/usr/bin/zsh|g" /mnt/etc/default/useradd
sed -i "s|/bin/bash|/usr/bin/zsh|g" /mnt/etc/passwd

# 配置Fstab
genfstab -U /mnt >> /mnt/etc/fstab

# 下载后续脚本
curl -o /mnt/step2.sh "https://raw.githubusercontent.com/Kirara17233/config/main/step2.sh"
curl -o /mnt/step3.sh "https://raw.githubusercontent.com/Kirara17233/config/main/step3.sh"
curl -o /mnt/usr/lib/systemd/system/install.service "https://raw.githubusercontent.com/Kirara17233/config/main/install.service"
chmod +x /mnt/step*.sh
sed -i "s|#rootpw|$1|g" /mnt/step*.sh
sed -i "s|#user|$2|g" /mnt/step*.sh
sed -i "s|#userpw|$3|g" /mnt/step*.sh
sed -i "s|#gitpw|$4|g" /mnt/step*.sh

# Chroot
arch-chroot /mnt /step2.sh

# 重启
umount /mnt/boot
umount /mnt
reboot
