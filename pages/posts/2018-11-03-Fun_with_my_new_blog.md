# Fun with my new blog

![Home Desktop](/assets/home-desktop.gif)

Welcome to my new blog! It is a while from last post. After having some free time from the work, I looked for something new to learn. I looked at the blog, a idea appeared: remaster the site.

Traditional blog platform such as WordPress is convenient and easy to use. However, it is overkill for a simple blog, and this code is ugly in developer's view. Some online blog platform such as [Medium](https://medium.com/) is good with clean layout and Markdown (yeah!). However, if we want to customize code, or live demo with injected JavaScript in the post, it is hopeless. 

The new site must satisfy these points:

* Lightweight, simple and serverless => HTML
* Clean content, easy to write => Markdown
* DRY, but flexible enough for customization 

At least, I also want to do something new, and fun with functional programming. The solution is writing a builder that export to HTML pages. I can mix Markdown for posts and HTML + JS for demo pages. The language of choice is PureScript, because it is faster to build, and much lightweight than Haskell.

I am a simple minimalist person. The exported HTML files is small as possible. Most of posts is CSS only, no JS. Just inject JavaScript code when needed. For now, the only page that use JS is home page, because I don't want the site is too boring, lol. 

The project is smoothly, except Markdown. The library [purescript-markdown-smolder](https://github.com/hgiasac/purescript-markdown-smolder) that converts from Markdown to HTML is old-dated to use. The dependency library [purescript-smolder](https://github.com/bodil/purescript-smolder) which is inspired from Blazer change to use Free Monad with breaking changes. And the author seem don't care about it anymore. So I must rewrite it.

I don't want to manually deploy each time updating new post. So I use Travis CI to automatically deploy to Github page. It's all, everything seems okay...

## But the site is boring!

It is boring with a static site. Maybe I need to do several UI effects. So I experiment Functional Reactive Programming (FRP). The background is drawn by canvas, and mouse behaviors to trigger rendering circles (top image). The site is also responsive with bias line.

![Home Mobile](/assets/home-mobile.gif)

With this, I hope the website is not too boring for you.
Anyway, thank for reading.






