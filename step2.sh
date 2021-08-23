#!/usr/bin/zsh

rootpw=#rootpw
user=#user
gitpw=#gitpw

# 设置时区
ln -sf /usr/share/zoneinfo/Europe/Moscow /etc/localtime
hwclock --systohc

# 链接neovim
ln -s /usr/bin/nvim /usr/bin/vi
ln -s /usr/bin/nvim /usr/bin/vim

# 开启pacman色彩选项
sed -i "s|#Color|Color|g" /etc/pacman.conf

# visudo
sed -i "s|# %wheel ALL=(ALL) ALL|%wheel ALL=(ALL) NOPASSWD:ALL|g" /etc/sudoers

# 配置
git clone --depth=1 https://Kirara17233:$gitpw@gitlab.com/Kirara17233/rsa.git /root/rsa
mkdir /etc/ssh/.ssh
cp /root/rsa/authorized_keys /etc/ssh/.ssh/authorized_keys
ln -s /etc/ssh/.ssh /etc/skel/.ssh

git clone --depth=1 https://github.com/ohmyzsh/ohmyzsh.git /etc/oh-my-zsh
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git /etc/oh-my-zsh/custom/themes/powerlevel10k
git clone --depth=1 https://github.com/zsh-users/zsh-syntax-highlighting.git /etc/oh-my-zsh/custom/plugins/zsh-syntax-highlighting
git clone --depth=1 https://github.com/zsh-users/zsh-autosuggestions.git /etc/oh-my-zsh/custom/plugins/zsh-autosuggestions
git clone --depth=1 https://gitlab.com/Kirara17233/config.git /root/config
cp /root/config/.p10k.zsh /etc/oh-my-zsh/.p10k.zsh
cp /root/config/.zshrc /etc/oh-my-zsh/.zshrc
ln -s /etc/oh-my-zsh/.zshrc /etc/skel/.zshrc
ln -s /etc/oh-my-zsh/.zshrc /root/.zshrc

mkdir /etc/xmonad
cp /root/config/xmonad.hs /etc/xmonad/xmonad.hs
mkdir /etc/skel/.xmonad
ln -s /etc/xmonad/xmonad.hs /etc/skel/.xmonad/xmonad.hs

rm -rf /root/rsa
rm -rf /root/config

# 设置Locale
sed -i "s|#en_US.UTF-8 UTF-8|en_US.UTF-8 UTF-8|g" /etc/locale.gen
sed -i "s|#zh_CN.UTF-8 UTF-8|zh_CN.UTF-8 UTF-8|g" /etc/locale.gen
locale-gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf

# 设置主机名
echo "Arch" > /etc/hostname
echo "127.0.0.1	localhost
::1		localhost
127.0.1.1	Arch.localdomain	Arch" >> /etc/hosts

# 设置Root密码
echo "root:$rootpw" | chpasswd

# 引导
pacman -S --noconfirm intel-ucode grub efibootmgr
grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=grub
grub-mkconfig -o /boot/grub/grub.cfg

# 配置网络
systemctl enable dhcpcd sshd

# 链接
systemctl enable install

# 退出chroot
exit
