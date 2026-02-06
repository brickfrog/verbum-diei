(function () {
  "use strict";

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

  function escapeHtml(value) {
    return String(value == null ? "" : value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/\"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  function escapeAttr(value) {
    return escapeHtml(value).replace(/`/g, "&#96;");
  }

  function isIsoDate(value) {
    return /^\d{4}-\d{2}-\d{2}$/.test(String(value || ""));
  }

  function kindLabel(kind) {
    return kind === "gospel" ? "Gospel" : "Reading";
  }

  function kindShort(kind) {
    return kind === "gospel" ? "G" : "R";
  }

  function lineId(kind, n) {
    return kind + "-L" + String(n);
  }

  function translationHref(translationId) {
    if (translationId === "dra") {
      return "https://www.gutenberg.org/ebooks/8300";
    }
    return "https://www.gutenberg.org/";
  }

  function normalizeDateList(values) {
    if (!Array.isArray(values)) {
      return [];
    }
    var out = [];
    for (var i = 0; i < values.length; i += 1) {
      var v = String(values[i] || "");
      if (isIsoDate(v)) {
        out.push(v);
      }
    }
    out.sort();
    out.reverse();
    return out;
  }

  function findDateFromPath(pathname) {
    var m = String(pathname || "").match(/\/d\/(\d{4}-\d{2}-\d{2})\/?$/);
    return m ? m[1] : null;
  }

  function listDataOrEmpty(value) {
    return Array.isArray(value) ? value : [];
  }

  function hrefLatest(assetPrefix) {
    return assetPrefix === "" ? "./" : assetPrefix;
  }

  function hrefArchive(assetPrefix) {
    return assetPrefix + "archive/";
  }

  function hrefDate(assetPrefix, date) {
    return hrefLatest(assetPrefix) + "?date=" + encodeURIComponent(date);
  }

  function fetchJson(url) {
    return fetch(url, { cache: "no-store" }).then(function (response) {
      if (!response.ok) {
        throw new Error("Request failed: " + response.status + " " + url);
      }
      return response.json();
    });
  }

  function readLabelForReading(reading, n) {
    if (!reading || !Array.isArray(reading.lineRefs)) {
      return String(n);
    }
    return String(reading.lineRefs[n - 1] || n);
  }

  function labelForKind(readingsByKind, kind, n) {
    return readLabelForReading(readingsByKind[kind], n);
  }

  function noteTarget(kind, lines) {
    if (!Array.isArray(lines) || lines.length === 0) {
      return "#";
    }
    return "#" + lineId(kind, lines[0]);
  }

  function noteHighlights(kind, lines) {
    if (!Array.isArray(lines)) {
      return "";
    }
    var ids = [];
    for (var i = 0; i < lines.length; i += 1) {
      ids.push(lineId(kind, lines[i]));
    }
    return ids.join(" ");
  }

  function renderLinesLabel(readingsByKind, kind, lines) {
    if (!Array.isArray(lines) || lines.length === 0) {
      return kindShort(kind);
    }
    var labels = [];
    for (var i = 0; i < lines.length; i += 1) {
      labels.push(labelForKind(readingsByKind, kind, lines[i]));
    }
    return kindShort(kind) + " " + labels.join(",");
  }

  function renderNavLink(href, label) {
    return (
      '<a class="site-link" href="' +
      escapeAttr(href) +
      '">' +
      escapeHtml(label) +
      "</a>"
    );
  }

  function renderReadingBox(reading) {
    var lines = listDataOrEmpty(reading.lines);
    var lineRefs = listDataOrEmpty(reading.lineRefs);
    var body = "";
    for (var i = 0; i < lines.length; i += 1) {
      var n = i + 1;
      var lineRef = String(lineRefs[i] || n);
      body +=
        '<p class="scripture-line" id="' +
        escapeAttr(lineId(reading.kind || "first", n)) +
        '"><span class="line-label">' +
        escapeHtml(lineRef) +
        '</span><span class="line-text">' +
        escapeHtml(lines[i]) +
        "</span></p>";
    }

    var translation = reading.translation || {};

    return (
      '<section class="panel reading-panel"><header class="panel-header"><div class="panel-kicker">' +
      escapeHtml(kindLabel(reading.kind)) +
      "</div><h2 class=\"panel-title\">" +
      escapeHtml(reading.heading || "") +
      '</h2><div class="panel-ref">' +
      escapeHtml(reading.reference || "") +
      '</div><div class="panel-meta">' +
      escapeHtml((translation.name || "") + " - " + (translation.note || "")) +
      '</div></header><article class="scripture-block">' +
      body +
      "</article></section>"
    );
  }

  function renderMarginalia(artifact, readingsByKind, hasLlm) {
    var notes = listDataOrEmpty(artifact.marginalia);
    if (notes.length === 0) {
      return (
        '<div class="empty-note">' +
        (hasLlm
          ? "(no marginalia generated)"
          : "LLM output unavailable. Set OPENAI_API_KEY and re-run bun run generate.") +
        "</div>"
      );
    }

    var items = "";
    for (var i = 0; i < notes.length; i += 1) {
      var note = notes[i] || {};
      var kind = note.readingKind || "first";
      var lines = listDataOrEmpty(note.lines);
      items +=
        '<li class="marginalia-item"><a class="note-ref" href="' +
        escapeAttr(noteTarget(kind, lines)) +
        '" data-hl="' +
        escapeAttr(noteHighlights(kind, lines)) +
        '">' +
        escapeHtml(renderLinesLabel(readingsByKind, kind, lines)) +
        '</a><span class="note-text">' +
        escapeHtml(note.text || "") +
        "</span></li>";
    }
    return '<ol class="marginalia-list">' + items + "</ol>";
  }

  function renderCommentNotes(readingsByKind, kind, notes) {
    var list = listDataOrEmpty(notes);
    if (list.length === 0) {
      return '<div class="empty-note">(no notes)</div>';
    }

    var items = "";
    for (var i = 0; i < list.length; i += 1) {
      var note = list[i] || {};
      var lines = listDataOrEmpty(note.lines);
      items +=
        '<li class="comment-item"><a class="note-ref" href="' +
        escapeAttr(noteTarget(kind, lines)) +
        '" data-hl="' +
        escapeAttr(noteHighlights(kind, lines)) +
        '">' +
        escapeHtml(renderLinesLabel(readingsByKind, kind, lines)) +
        '</a><span class="note-text">' +
        escapeHtml(note.text || "") +
        "</span></li>";
    }

    return '<ul class="comment-list">' + items + "</ul>";
  }

  function renderCommentary(artifact, readingsByKind, hasLlm) {
    var commentary = artifact.commentary || {};
    var readingNotes = listDataOrEmpty(commentary.reading);
    var gospelNotes = listDataOrEmpty(commentary.gospel);
    var synthesis = String(commentary.synthesis || "").trim();
    var excursus = String(commentary.excursus || "").trim();
    var seminaVerbi = String(commentary.seminaVerbi || "").trim();
    var emptyText = hasLlm
      ? "(no commentary generated)"
      : "LLM output unavailable. Set OPENAI_API_KEY and re-run bun run generate.";

    var isEmpty = readingNotes.length === 0 && gospelNotes.length === 0 && synthesis === "";

    var columns = isEmpty
      ? '<div class="empty-note">' + emptyText + "</div>"
      : '<div class="commentary-columns">' +
        '<section class="commentary-column"><div class="panel-kicker">On the Reading</div>' +
        renderCommentNotes(readingsByKind, "first", readingNotes) +
        "</section>" +
        '<section class="commentary-column"><div class="panel-kicker">On the Gospel</div>' +
        renderCommentNotes(readingsByKind, "gospel", gospelNotes) +
        "</section>" +
        "</div>";

    var synthesisNode =
      synthesis === ""
        ? ""
        : '<p class="doctrinal-synthesis"><span class="meta-label">Doctrinal</span> ' +
          escapeHtml(synthesis) +
          "</p>";

    var excursusText =
      excursus === "" ? (hasLlm ? "(no heterodox reading generated)" : emptyText) : excursus;

    var seminaText =
      seminaVerbi === "" ? (hasLlm ? "(no semina verbi generated)" : emptyText) : seminaVerbi;

    return (
      '<section class="panel commentary-panel"><header class="panel-header"><div class="panel-kicker">Gloss</div><h2 class="panel-title">Commentary</h2></header>' +
      columns +
      synthesisNode +
      '<section class="supplement-panel"><div class="panel-kicker panel-kicker-strong">Heterodox Reading</div><div class="supplement-text">' +
      escapeHtml(excursusText) +
      "</div></section>" +
      '<section class="supplement-panel"><div class="panel-kicker panel-kicker-strong">Semina Verbi</div><div class="supplement-text">' +
      escapeHtml(seminaText) +
      "</div></section></section>"
    );
  }

  function renderObservances(artifact) {
    var meta = (artifact.observances && artifact.observances.meta) || {};
    var celebrations = listDataOrEmpty(artifact.observances && artifact.observances.celebrations);

    var celebrationNodes = "";
    for (var i = 0; i < celebrations.length; i += 1) {
      var celebration = celebrations[i] || {};
      celebrationNodes +=
        '<li class="celebration-item"><span class="celebration-rank">' +
        escapeHtml(celebration.rank || "") +
        '</span><span class="celebration-name">' +
        escapeHtml(celebration.name || "") +
        "</span></li>";
    }

    return (
      '<section class="panel observances-panel"><header class="panel-header"><div class="panel-kicker">Day Office</div><h2 class="panel-title">Observances</h2></header>' +
      '<div class="observance-meta-grid">' +
      '<div class="meta-cell"><span class="meta-label">Season</span><span class="meta-value">' +
      escapeHtml(meta.season || "") +
      '</span></div><div class="meta-cell"><span class="meta-label">Cycle</span><span class="meta-value">' +
      escapeHtml(meta.cycle || "") +
      '</span></div><div class="meta-cell"><span class="meta-label">Psalter</span><span class="meta-value">' +
      escapeHtml(meta.psalterWeek || "") +
      "</span></div></div>" +
      '<ul class="celebration-list">' +
      celebrationNodes +
      "</ul></section>"
    );
  }

  function renderDay(root, artifact, dates, assetPrefix) {
    var readings = listDataOrEmpty(artifact.readings);
    var readingsByKind = { first: null, gospel: null };
    for (var i = 0; i < readings.length; i += 1) {
      var r = readings[i] || {};
      if (r.kind === "gospel") {
        readingsByKind.gospel = r;
      } else if (!readingsByKind.first) {
        readingsByKind.first = r;
      }
    }

    var readingNodes = "";
    if (readingsByKind.first) {
      readingNodes += renderReadingBox(readingsByKind.first);
    }
    if (readingsByKind.gospel) {
      readingNodes += renderReadingBox(readingsByKind.gospel);
    }

    var firstReading = readingsByKind.first || readings[0] || null;
    var translation = firstReading ? firstReading.translation || {} : {};
    var translationLabel = firstReading
      ? (translation.name || "Scripture") + " (" + String(translation.id || "").toUpperCase() + ")"
      : "Scripture";

    var source = artifact.source || {};
    var hasLlm =
      !!artifact.llm && Array.isArray(artifact.llm.calls) && artifact.llm.calls.length > 0;

    var heroLinks = "";
    if (source.itemUrl) {
      heroLinks += renderNavLink(source.itemUrl, "Vatican News");
    }
    heroLinks += renderNavLink(hrefLatest(assetPrefix), "Latest");
    heroLinks += renderNavLink(hrefArchive(assetPrefix), "Archive");
    heroLinks += renderNavLink(hrefDate(assetPrefix, artifact.date), "Permalink");

    root.className = "cathedral-layout";
    root.innerHTML =
      '<header class="hero-panel"><div class="hero-title-wrap"><div class="hero-kicker">Daily Office of the Word</div><h1 class="hero-title">Verbum Diei</h1><div class="hero-date">' +
      escapeHtml(artifact.date || "") +
      "</div></div><nav class=\"hero-nav\">" +
      heroLinks +
      "</nav></header>" +
      renderObservances(artifact) +
      '<aside class="panel marginalia-panel"><header class="panel-header"><div class="panel-kicker">Margin</div><h2 class="panel-title">Marginalia</h2></header>' +
      renderMarginalia(artifact, readingsByKind, hasLlm) +
      "</aside>" +
      '<section class="reading-stack">' +
      readingNodes +
      "</section>" +
      renderCommentary(artifact, readingsByKind, hasLlm) +
      '<footer class="site-footer"><p class="footer-note">Scripture text: <a class="site-link" href="' +
      escapeAttr(translationHref(translation.id || "")) +
      '">' +
      escapeHtml(translationLabel) +
      '</a> - public domain. For the official source see Vatican News above.</p><p class="footer-note">Marginalia and glosses are generated by a language model and offered for reflection, not doctrinal instruction.</p><p class="footer-note">Source code: <a class="site-link" href="https://github.com/brickfrog/verbum-diei">github.com/brickfrog/verbum-diei</a></p></footer>';

    document.title = "Verbum Diei - " + (artifact.date || "");
  }

  function renderArchive(root, dates, assetPrefix) {
    var items = "";
    for (var i = 0; i < dates.length; i += 1) {
      var date = dates[i];
      items +=
        '<li class="archive-item"><a class="site-link archive-link" href="' +
        escapeAttr(hrefDate(assetPrefix, date)) +
        '">' +
        escapeHtml(date) +
        "</a></li>";
    }

    if (items === "") {
      items = '<li class="archive-item"><span class="empty-note">No generated days yet.</span></li>';
    }

    root.className = "cathedral-layout archive-layout";
    root.innerHTML =
      '<header class="hero-panel archive-hero"><div class="hero-title-wrap"><div class="hero-kicker">All Days</div><h1 class="hero-title">Archive</h1></div><nav class="hero-nav">' +
      renderNavLink(hrefLatest(assetPrefix), "Latest") +
      '</nav></header><section class="panel archive-panel"><header class="panel-header"><div class="panel-kicker">Chronicle</div><h2 class="panel-title">Days</h2></header><ul class="archive-list">' +
      items +
      '</ul></section><footer class="site-footer"><p class="footer-note">Generated daily from Vatican News RSS + local calendar data.</p><p class="footer-note">Source code: <a class="site-link" href="https://github.com/brickfrog/verbum-diei">github.com/brickfrog/verbum-diei</a></p></footer>';

    document.title = "Verbum Diei - Archive";
  }

  function renderError(root, message) {
    root.className = "cathedral-layout archive-layout";
    root.innerHTML =
      '<section class="panel archive-panel"><header class="panel-header"><div class="panel-kicker">Error</div><h2 class="panel-title">Could not load page data</h2></header><div class="empty-note">' +
      escapeHtml(message) +
      "</div></section>";
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

  var root = document.getElementById("app-root");
  if (!root) {
    return;
  }

  var assetPrefix = root.getAttribute("data-asset-prefix") || "";
  var defaultView = root.getAttribute("data-default-view") || "latest";

  fetchJson(assetPrefix + "data/archive.json")
    .then(function (archivePayload) {
      var dates = normalizeDateList(archivePayload && archivePayload.dates);
      var params = new URLSearchParams(location.search);
      var dateFromQuery = params.get("date");
      var dateFromPath = findDateFromPath(location.pathname);
      var targetDate = null;

      if (isIsoDate(dateFromQuery)) {
        targetDate = dateFromQuery;
      } else if (isIsoDate(dateFromPath)) {
        targetDate = dateFromPath;
      } else if (defaultView !== "archive" && dates.length > 0) {
        targetDate = dates[0];
      }

      if (!targetDate) {
        renderArchive(root, dates, assetPrefix);
        return null;
      }

      return fetchJson(assetPrefix + "data/" + targetDate + ".json")
        .then(function (artifact) {
          renderDay(root, artifact || {}, dates, assetPrefix);
        })
        .catch(function () {
          renderError(root, "No day data found for " + targetDate + ".");
        });
    })
    .catch(function (err) {
      renderError(root, err && err.message ? err.message : "Unknown error.");
    });
})();
