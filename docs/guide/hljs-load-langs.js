(function loadLangs() {
  // mdbook is not very extensible yet. This is a workaround.
  // In the long term, we should probably switch to org-mode.
  // See https://github.com/rust-lang/mdBook/issues/657#issuecomment-377935449.

  function highlight(lang) {
    Array
      .from(document.querySelectorAll("code.hljs.language-" + lang))
      .forEach(function (block) { hljs.highlightBlock(block); })
  }

  function load(lang) {
    var src = "//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.1/languages/" + lang + ".min.js";
    var s = document.createElement('script');
    s.setAttribute('src', src);
    s.setAttribute('type', 'text/javascript');
    s.setAttribute('charset', 'utf-8');
    s.onload = function() {
      highlight(lang);
    };
    document.body.appendChild(s);
  }

  load('lisp');
})();
