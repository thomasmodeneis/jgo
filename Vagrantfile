# JGo Vagrant Build
#
#

box      = 'jgo-ubuntu12-64b'
url      = 'https://cloud-images.ubuntu.com/vagrant/precise/current/precise-server-cloudimg-amd64-vagrant-disk1.box'
hostname = 'jgo'
domain   = 'jgo.org'
ip       = '192.168.0.42'
ram      = '2048'

Vagrant::Config.run do |config|
  config.vm.box = box
  config.vm.box_url = url
  config.vm.host_name = hostname + '.' + domain
  config.vm.network :hostonly, ip

  config.vm.customize [
    'modifyvm', :id,
    '--name', hostname,
    '--memory', ram
  ]

  config.vm.provision :shell, :run => "always" do |shell|
  shell.inline = %{

    #install puppet
    echo -e "deb http://apt.puppetlabs.com/ lucid main\ndeb-src http://apt.puppetlabs.com/ lucid main" >> /etc/apt/sources.list.d/puppet.list
    apt-key adv --keyserver keyserver.ubuntu.com --recv 4BD6EC30
    apt-get update --fix-missing --force-yes -y
    apt-get install puppet --fix-missing --force-yes -y

    #install openjdk 8
    sudo add-apt-repository ppa:openjdk-r/ppa -y
    sudo apt-get update -y
    sudo apt-get install openjdk-8-jdk  -y

    #install sbt
    echo "deb http://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
    sudo apt-get update  -y
    sudo apt-get install sbt  -y --force-yes

  }
  end

  config.vm.provision :puppet do |puppet|
    puppet.manifests_path = 'puppet/manifests'
    puppet.manifest_file = 'site.pp'
    puppet.module_path = 'puppet/modules'
  end

end

Vagrant.configure("2") do |config|
  #download and build openjdk8
  #run get_source.sh make configure and make images
  config.vm.provision "shell", path: "after_provision.sh", privileged: false

  config.vm.synced_folder "src/", "/opt/jgo"

end

