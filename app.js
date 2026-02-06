(function () {
  var HIGHLIGHT_CLASS = "is-highlighted";
  var highlighted = [];

  function clearHighlights() {
    for (var i = 0; i < highlighted.length; i += 1) {
      highlighted[i].classList.remove(HIGHLIGHT_CLASS);
    }
    highlighted = [];
  }

  function applyHighlights(ids) {
    clearHighlights();
    for (var i = 0; i < ids.length; i += 1) {
      var el = document.getElementById(ids[i]);
      if (el) {
        el.classList.add(HIGHLIGHT_CLASS);
        highlighted.push(el);
      }
    }
  }

  document.addEventListener(
    "click",
    function (event) {
      var target = event.target;
      if (!target || typeof target.closest !== "function") {
        return;
      }
      var link = target.closest("a.note-ref");
      if (!link) {
        return;
      }
      var raw = link.getAttribute("data-hl");
      if (!raw) {
        return;
      }
      var ids = raw.split(/\s+/).filter(Boolean);
      if (ids.length > 0) {
        applyHighlights(ids);
      }
    },
    true,
  );
})();
