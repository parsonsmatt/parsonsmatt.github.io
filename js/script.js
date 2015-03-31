---

---


document.addEventListener("DOMContentLoaded", function(event) {
    var hidden = true;
    var menu = document.querySelector(".main-nav");
    var opener = document.querySelector(".opener");
    opener.addEventListener("click", function(e){
        e.preventDefault();
        if(hidden){
            menu.setAttribute("class", "main-nav active-menu");
        }else{
            menu.setAttribute("class", "main-nav");
        }
        hidden = !hidden;
    });

    var cmtBtn = document.getElementById("showComments");
    if(!cmtBtn){
        return;
    }

    var commentHidden = true;
    var disqus_shortname = '';
    var dt = document.getElementById("disqus_thread");

    cmtBtn.addEventListener("click", function(e){
        e.preventDefault();
        if(disqus_shortname === ''){
            disqus_shortname = '{{ site.disqus }}';
            dt.innerHTML = 'Loading comments...';
            (function() {
                var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
                dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
                (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
            })();
        }
        if(hidden){
            dt.setAttribute('style','display:block');
            this.innerHTML = 'Hide Comments';
        }else{
            dt.setAttribute('style','display:none');
            this.innerHTML = 'Comments';
        }
        hidden = !hidden;
    });
});
