# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "shill/freebsd-9.3"
  config.vm.synced_folder ".", "/home/vagrant/shill", type: "rsync"

  config.vm.provider "virtualbox" do |vb|
     vb.gui = false
     vb.memory = "1024"
  end

  config.vm.provision "shell", inline: <<-SHELL
    fetch -o /tmp/racket-6.6-src-builtpkgs.tgz https://mirror.racket-lang.org/installers/6.6/racket-6.6-src-builtpkgs.tgz
    cd /tmp
    tar xvf racket-6.6-src-builtpkgs.tgz
    cd racket-6.6
    mkdir build
    cd build
    ../src/configure --prefix=/usr/local --enable-shared && make && make install
    cd /tmp && rm -Rf /tmp/*
  SHELL

  config.vm.provision "shell", privileged: false, inline: <<-SHELL
    cd /home/vagrant/shill/racket && raco link -n shill .
    cd /home/vagrant/shill/stdlib && raco link -n shill .
    cd /home/vagrant/shill && make
  SHELL

  config.vm.provision "shell", inline: <<-SHELL
    cd /home/vagrant/shill && make install
    echo 'shill_load="YES"' >> /boot/loader.conf
  SHELL

  config.vm.provision :reload
end
