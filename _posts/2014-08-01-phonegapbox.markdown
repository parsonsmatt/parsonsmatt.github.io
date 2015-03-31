---
layout: post
title:  "Vagrant, Windows, and Phonegap: A tale of woe"
date:   2014-08-01 19:00:00
categories: programming
---

A project I'm working on is converting from a [Phonegap Build](https://build.phonegap.com/) to a locally built project, thanks to the lack of plugins available to Build compared to the much larger library of plugins available to the offline platform.

I'm pretty sure I know why Phonegap Build is useful -- Phonegap is a total PITA to setup and configure, especially in a command line only situation. There are a huge nest of dependencies and most of the answers to fix things are hidden in StackOverflow threads and bug reports in the official documentation for various tools. This is exactly the kind of situation where [Vagrant](http://www.vagrantup.com) becomes useful -- do the work to make a working dev box *once* and then share the result out. This'll let me develop on both desktop and laptop with no issue, and integrating new folks into the team will be easy.

So, let's put in the work once, so I don't have to do it again!

## Getting it running

I'm basing the box off of the `ubuntu\trusty64` box, since it seems to be kept up to date fairly well by the Ubuntu folks. I added the standard `config.vm.provision :shell, path: "bootstrap.sh"` to get a provisioning script running, and forwarded port 3000 on the guest to 3000 on the host to allow for use of the excellent `phonegap serve` for live debugging and testing.

## The Bootstrap.sh

Developing with Android and Phonegap is kind of a mess since everything has *so many dependencies*. It's terrible, really. If it weren't so useful, I wouldn't even bother. Here's the bootstrap file I came up with:

{% highlight bash %}
#!/bin/sh

# Install nodejs and NPM
sudo add-apt-repository ppa:chris-lea/node.js -y
sudo dpkg --add-architecture i386
sudo apt-get update -y
sudo apt-get install nodejs -y
sudo npm install npm -g

# Install phonegap and phonegap plugin manager
sudo npm install phonegap -g
sudo npm install plugman -g

# Install Java SDK
sudo apt-get install openjdk-7-jdk -y

# Install Android SDK
wget http://dl.google.com/android/android-sdk_r23.0.2-linux.tgz
tar -xzf android-sdk_r23.0.2-linux.tgz
sudo apt-get install expect -y # allows to give Y to license prompt
sudo apt-get install libncurses5:i386 libstdc++6:i386 zlib1g:i386 -y
sudo apt-get update
sudo apt-get install libncurses5:i386 libstdc++6:i386 zlib1g:i386
expect -c '
set timeout -1   ;
spawn sudo android-sdk-linux/tools/android update sdk --no-ui; 
expect { 
    "Do you accept the license" { exp_send "y\r" ; exp_continue }
    eof
}
'
{% endhighlight %}

The `expect` block was pulled from [this very helpful SO answer](http://stackoverflow.com/a/17863931/3780203), and works great.

Of course, this doesn't work. It gets pretty far, though! You'll be able to create an app and run `phonegap serve` and everything will look awesome. But when you try to do a `run` or `build` it crashes out with an error around shelljs. Unable to debug the matter, I threw in the towel for the night.

Fortunately, someone else has begun working on a [box](https://github.com/vasconcelloslf/phonegap-box) which is much farther along, so I decided to start working on this to try and get it going. When I download and test it out, I get the same problem -- so something nefarious must be up. According to [this SO question](http://stackoverflow.com/questions/19592701/phonegap-building-phonegap-android-app-gives-compile-error-on-linux), doing a `cordova platform remove android` and `cordova platform add android` should fix it. However, I just get another error with the key line:

{% highlight bash %}
...
at Object.fs.symlinkSync (fs.js:735:18)
...
{% endhighlight %}

Hmm... So a symlink problem? Searching Google for that brings up [this SO post](http://stackoverflow.com/questions/24200333/symbolic-links-and-synced-folders-in-vagrant) with the following answer:

> Virtualbox does not allow symlinks on shared folders for security reasons. To enable symlinks the following line needs to be added to the vm provider config block in the Vagrantfile:
> {% highlight ruby %}
config.vm.provider "virtualbox" do |v|
    v.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]
end
{% endhighlight %}
> Additionally, on windows vagrant up needs to be executed in a shell with admin rights. No workarounds necessary.

> source: https://coderwall.com/p/qklo9w

Wonderful. This is exactly what I need, even if I hate the requirement of running a shell with admin rights. There's an option in `secpol.msc` to give users the "Create Symlink" permission which I will experiment with next. Unfortunately, while it's necessary that your user account be added to this group, that doesn't remove the need to run the shell as administrator.

The results of my work here have been merged into [this Github repo](https://github.com/vasconcelloslf/phonegap-box), which is now working great for me as I continue working on my Phonegap application.