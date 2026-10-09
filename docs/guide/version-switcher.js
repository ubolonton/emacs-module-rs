// Adds a version menu to the menu bar. CI publishes each version to <site>/<version>/, next to
// `latest` and versions.json, which lists the versions, newest first. Local builds have no
// versions.json, so they show no menu.
(function versionSwitcher() {
  // `path_to_root` is empty for top-level pages. The 404 page sets <base> to the book root, so
  // resolve against the base URI, not the page URL.
  const bookRoot = new URL(path_to_root || "./", document.baseURI);
  const siteRoot = new URL("../", bookRoot);
  const current = bookRoot.pathname.split("/").filter(Boolean).pop();
  const pagePath = window.location.href.startsWith(bookRoot.href)
    ? window.location.href.slice(bookRoot.href.length)
    : "";

  fetch(new URL("versions.json", siteRoot))
    .then((response) => (response.ok ? response.json() : []))
    .then((versions) => {
      if (versions.length === 0) {
        return;
      }
      const select = document.createElement("select");
      select.className = "version-switcher";
      select.title = "Version";
      select.setAttribute("aria-label", "Version");
      for (const version of ["latest", ...versions]) {
        select.add(new Option(version, version, false, version === current));
      }
      // Older versions may lack the page. Their 404 page links to their index.
      select.addEventListener("change", () => {
        window.location.href = new URL(`${select.value}/${pagePath}`, siteRoot).href;
      });
      document.querySelector(".right-buttons").prepend(select);
    })
    // Without the menu, the page still works.
    .catch(() => {});
})();
