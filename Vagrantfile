# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = if ENV.has_key?("SHILL_GUI")
                  then "shill/freebsd-9.3-mate"
                  else "shill/freebsd-9.3"
                  end
  config.vm.synced_folder ".", "/home/vagrant/shill", type: "rsync"

  config.vm.provider "virtualbox" do |vb|
    vb.gui = ENV.has_key?("SHILL_GUI")
    vb.memory = "1024"
  end

  config.vm.provision "shell", inline: <<-SHELL
    pkg install -y racket
    cd /usr/src && rm -Rf *
    fetch https://codeload.github.com/HarvardPL/ShillBSD/zip/release/9.3.0
    unzip 9.3.0
    mv ShillBSD-release-9.3.0/* .
    rm -Rf 9.3.0 ShillBSD-release-9.3.0
  SHELL

  config.vm.provision "shell", privileged: false, inline: <<-SHELL
    cd /home/vagrant/shill/racket && sudo raco link -i -n shill .
    cd /home/vagrant/shill/stdlib && sudo raco link -i -n shill .
    cd /home/vagrant/shill && make && sudo make install
    sudo sh -c 'echo shill_load="YES" >> /boot/loader.conf'
  SHELL

  config.vm.provision :reload
end
