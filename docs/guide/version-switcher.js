// Adds a version menu to the menu bar. The publish task puts this file at the site root, next to
// each version's directory, `latest`, and versions.json, which lists the versions, newest first,
// with their pages.
(function versionSwitcher() {
  // `path_to_root` is empty for top-level pages. The 404 page sets <base> to the book root, so
  // resolve against the base URI, not the page URL.
  const bookRoot = new URL(path_to_root || "./", document.baseURI);
  const siteRoot = new URL("../", bookRoot);
  const current = bookRoot.pathname.split("/").filter(Boolean).pop();
  const pagePath = window.location.pathname.startsWith(bookRoot.pathname)
    ? window.location.pathname.slice(bookRoot.pathname.length) || "index.html"
    : "index.html";

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
      // `latest` is a link to the newest minor version, so it has the same pages.
      const latest = versions.find((version) => version.name !== "master");
      const entries = latest ? [{ ...latest, name: "latest" }, ...versions] : versions;
      // On the 404 page, the current version lacks the page too, so don't disable any version.
      const currentEntry = entries.find((version) => version.name === current);
      const pageExists = !currentEntry || currentEntry.pages.includes(pagePath);
      for (const version of entries) {
        const option = new Option(version.name, version.name, false, version.name === current);
        option.disabled = pageExists && !version.pages.includes(pagePath);
        select.add(option);
      }
      select.addEventListener("change", () => {
        const url = new URL(`${select.value}/${pagePath}`, siteRoot);
        url.hash = window.location.hash;
        window.location.href = url.href;
      });
      document.querySelector(".right-buttons").prepend(select);
    })
    // Without the menu, the page still works.
    .catch(() => {});
})();
