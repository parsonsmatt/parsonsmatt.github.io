---
title: "Protecting my Javascript"
date: 2014-07-26 12:00:00
layout: post
category: programming
---

## Well, what do we have here?

Good OO practice is to encapsulate your data. I recently wrote a script that managed some data and methods for a mobile application to write WiFi information to an NFC. My first design looked something like this:

{% highlight javascript linenos %}
var SSID;
var pass;

function formatNetwork() {
    toWrite= {};
    toWrite['SSID'] = SSID;
    toWrite['pass'] = pass;
    return JSON.stringify(toWrite);
}
{% endhighlight %}

Except this is terrible: `SSID` and `pass` are global variables and exposed to the whole world. Additionally, the network formatting is fragile and difficult to expand. I decided to upgrade to the following and store everything in an object.

## Object Time

{% highlight javascript linenos %}
var wifiWriter = {
    SSID: 'none',
    pass: 'none',
    formatNetwork: function() {
        var toWrite = {};
        for (var prop in wifiWriter) {
            if (!isFunction(prop)) {
                toWrite[prop] = wifiWriter[prop];
            }
        }
        return JSON.stringify(toWrite);
    }
}
// isFunction() was pulled from UnderscoreJS
{% endhighlight %}

Much better. There's only a single global variable now. The `formatNetwork` function checks all of the non-variable properties of the `wifiWriter` object and adds them to the object, so we can add more fields to the string without breaking everything. I did end up adding a field, so this paid off sooner rather than later. 

It's still not encapsulated. Anyone can access the two variables and modify them directly. My OO training tells me that this is horrible, and I don't trust myself not to screw things up somehow. We'll encapsulate it in a function for best results, returning an object that has methods to access and modify the variables.

## It's Function Time

{% highlight javascript linenos %}
var wifiWriter = function () {
    var SSID = 'none';
    var pass = 'none';
    
    return {
        getSSID: function() {
            return SSID;
        },
        setSSID: function(newSSID) {
            // Validation goes here right?
            SSID = newSSID;
        }, // pass get/set left as exercise to reader
        formatNetwork: function() {
            var toWrite = {};
            for (var prop in wifiWriter) {
                if (!isFunction(prop)) {
                    toWrite[prop] = wifiWriter[prop];
                }
            }
            return JSON.stringify(toWrite);
        }
    }; // trick #1: end the return statement
}(); // trick #2: call function immediately after declaring it
{% endhighlight %}

PROBLEM. My clever way of looping through the variables no longer works, since its a function and not an object anymore. It might be possible to pull the variables out in a similar manner, but you know, what if I wanted to add additional fields that weren't to be incorporated? Let's wrap it an object!

{% highlight javascript linenos %}
var wifiWriter = function () {
    var wifiInfo = { 
        SSID: 'none', 
        pass: 'none', 
        locationID: 'where even' 
    };
    
    return {
        getSSID: function() {
            return SSID;
        },
        setSSID: function(newSSID) {
            // Validation goes here right?
            SSID = newSSID;
        }, // pass get/set left as exercise to reader
        formatNetwork: function() {
            var toWrite = {}
            for (var prop in wifiInfo) {
                if (!isFunction(prop)) {
                    toWrite[prop] = wifiInfo[prop];
                }
            }
            return JSON.stringify(toWrite);
        }
    };
}();
{% endhighlight %}

This satisfies my need for good code writing. 
