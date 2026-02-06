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

  var OFFICE_SCHEDULE = [
    {
      key: "matins",
      label: "Matins",
      hourLocal: 0,
      minuteLocal: 0,
      prayer: "Lord, open my lips, and my mouth shall declare your praise.",
    },
    {
      key: "lauds",
      label: "Lauds",
      hourLocal: 6,
      minuteLocal: 0,
      prayer: "Blessed are you, Lord, in the light of the new day.",
    },
    {
      key: "terce",
      label: "Terce",
      hourLocal: 9,
      minuteLocal: 0,
      prayer: "Come, Holy Spirit, and lighten our work in truth.",
    },
    {
      key: "sext",
      label: "Sext",
      hourLocal: 12,
      minuteLocal: 0,
      prayer: "God, come to my assistance. Lord, make haste to help me.",
    },
    {
      key: "none",
      label: "None",
      hourLocal: 15,
      minuteLocal: 0,
      prayer: "Stay with us, Lord, in the heat and trial of this day.",
    },
    {
      key: "vespers",
      label: "Vespers",
      hourLocal: 18,
      minuteLocal: 0,
      prayer: "Let my prayer rise before you like incense this evening.",
    },
    {
      key: "compline",
      label: "Compline",
      hourLocal: 21,
      minuteLocal: 0,
      prayer: "Into your hands, Lord, I commend my spirit.",
    },
  ];

  function readTimeZone() {
    if (typeof Intl !== "undefined" && Intl.DateTimeFormat) {
      try {
        var tz = Intl.DateTimeFormat().resolvedOptions().timeZone;
        if (tz) {
          return tz;
        }
      } catch (_err) {
        // Fall through to UTC.
      }
    }
    return "UTC";
  }

  function parseIsoDateParts(dateIso) {
    var m = String(dateIso || "").match(/^(\d{4})-(\d{2})-(\d{2})$/);
    if (!m) {
      return null;
    }
    return {
      year: Number(m[1]),
      month: Number(m[2]),
      day: Number(m[3]),
    };
  }

  function pad2(n) {
    return n < 10 ? "0" + String(n) : String(n);
  }

  function readDateFromPage() {
    var heroDate = document.querySelector(".hero-date");
    if (heroDate) {
      var text = String(heroDate.textContent || "").trim();
      if (isIsoDate(text)) {
        return text;
      }
    }
    var now = new Date();
    return (
      String(now.getUTCFullYear()) +
      "-" +
      pad2(now.getUTCMonth() + 1) +
      "-" +
      pad2(now.getUTCDate())
    );
  }

  function formatLocalClock(hour, minute) {
    var hour12 = hour % 12;
    if (hour12 === 0) {
      hour12 = 12;
    }
    var suffix = hour >= 12 ? "PM" : "AM";
    return String(hour12) + ":" + pad2(minute) + " " + suffix;
  }

  function officeMinuteLocalFromRow(row) {
    if (!row || typeof row.getAttribute !== "function") {
      return null;
    }
    var hourRaw = row.getAttribute("data-hour-local");
    var minuteRaw = row.getAttribute("data-minute-local");
    if (hourRaw == null || minuteRaw == null) {
      // Backward compatibility for older generated pages.
      hourRaw = row.getAttribute("data-hour-utc");
      minuteRaw = row.getAttribute("data-minute-utc");
    }
    var hour = Number(hourRaw);
    var minute = Number(minuteRaw);
    if (!Number.isFinite(hour) || !Number.isFinite(minute)) {
      return null;
    }
    return hour * 60 + minute;
  }

  function currentOfficeIndexLocal(rows) {
    if (!Array.isArray(rows) || rows.length === 0) {
      return -1;
    }
    var now = new Date();
    var nowMinuteLocal = now.getHours() * 60 + now.getMinutes();
    var index = rows.length - 1;
    for (var i = 0; i < rows.length; i += 1) {
      var officeMinute = officeMinuteLocalFromRow(rows[i]);
      if (officeMinute != null && officeMinute <= nowMinuteLocal) {
        index = i;
      }
    }
    return index;
  }

  function enhanceHoursPanel() {
    var panel = document.getElementById("hours-of-prayer");
    if (!panel) {
      return;
    }

    var rows = Array.prototype.slice.call(panel.querySelectorAll(".hour-row"));
    if (rows.length === 0) {
      return;
    }

    var timeZone = readTimeZone();
    var meta = panel.querySelector(".panel-meta");
    if (meta) {
      meta.textContent = "Times shown for " + timeZone + ".";
    }

    for (var i = 0; i < rows.length; i += 1) {
      var row = rows[i];
      var hourRaw = row.getAttribute("data-hour-local");
      var minuteRaw = row.getAttribute("data-minute-local");
      if (hourRaw == null || minuteRaw == null) {
        hourRaw = row.getAttribute("data-hour-utc");
        minuteRaw = row.getAttribute("data-minute-utc");
      }
      var hour = Number(hourRaw);
      var minute = Number(minuteRaw);
      var timeNode = row.querySelector(".hour-time");
      if (timeNode && Number.isFinite(hour) && Number.isFinite(minute)) {
        timeNode.textContent = formatLocalClock(hour, minute);
      }
      row.classList.remove("is-current");
    }

    var currentIndex = currentOfficeIndexLocal(rows);
    if (currentIndex >= 0 && currentIndex < rows.length) {
      rows[currentIndex].classList.add("is-current");
    }
  }

  function formatOfficeTime(dateIso, hourUtc, minuteUtc, timeZone) {
    var parts = parseIsoDateParts(dateIso);
    if (!parts) {
      return "--";
    }
    var instant = new Date(
      Date.UTC(parts.year, parts.month - 1, parts.day, hourUtc, minuteUtc, 0),
    );
    try {
      return new Intl.DateTimeFormat("en-US", {
        hour: "numeric",
        minute: "2-digit",
        hour12: true,
        timeZone: timeZone,
      }).format(instant);
    } catch (_err) {
      return new Intl.DateTimeFormat("en-US", {
        hour: "numeric",
        minute: "2-digit",
        hour12: true,
        timeZone: "UTC",
      }).format(instant);
    }
  }

  function summarizeOneLine(text, fallback) {
    var raw = String(text || "").trim().replace(/\s+/g, " ");
    if (raw === "") {
      return fallback;
    }
    var clipped = raw.slice(0, 140);
    return clipped.length < raw.length ? clipped.replace(/[,:;.!?]?\s*$/, "") + "..." : clipped;
  }

  function findDateFromPath(pathname) {
    var path = String(pathname || "").replace(/\/+$/, "");
    var permalink = path.match(/\/d\/(\d{4}-\d{2}-\d{2})$/);
    if (permalink) {
      return permalink[1];
    }
    var legacy = path.match(/\/(\d{4}-\d{2}-\d{2})$/);
    if (legacy) {
      return legacy[1];
    }
    return null;
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
    return assetPrefix + "d/" + encodeURIComponent(date) + "/";
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
    var panelId = reading.kind === "gospel" ? "reading-gospel" : "reading-first";

    return (
      '<section class="panel reading-panel" id="' +
      escapeAttr(panelId) +
      '"><header class="panel-header"><div class="panel-kicker">' +
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
    var meta = (artifact.observances && artifact.observances.meta) || {};
    var celebrations = listDataOrEmpty(artifact.observances && artifact.observances.celebrations);
    var source = artifact.source || {};
    var commentary = artifact.commentary || {};
    var notes = listDataOrEmpty(artifact.marginalia);

    var contextItems = "";
    contextItems +=
      '<li><span class="meta-label">Season</span> ' +
      escapeHtml(meta.season || "Ordinary Time") +
      "</li>";
    contextItems +=
      '<li><span class="meta-label">Cycle</span> ' + escapeHtml(meta.cycle || "Year A") + "</li>";
    contextItems +=
      '<li><span class="meta-label">Psalter</span> ' +
      escapeHtml(meta.psalterWeek || "Week I") +
      "</li>";
    if (celebrations.length > 0) {
      contextItems +=
        '<li><span class="meta-label">Saint</span> ' +
        escapeHtml(celebrations[0].name || "") +
        "</li>";
    }
    if (source.itemUrl) {
      contextItems +=
        '<li><span class="meta-label">Source</span> <a class="meta-link" href="' +
        escapeAttr(source.itemUrl) +
        '">Vatican News</a></li>';
    }

    var promptItems = "";
    promptItems +=
      "<li>" +
      escapeHtml(
        summarizeOneLine(
          commentary.synthesis,
          "Hold one line in memory and revisit it before sleep.",
        ),
      ) +
      "</li>";
    promptItems +=
      "<li>" +
      escapeHtml(
        summarizeOneLine(
          commentary.excursus,
          "Name one attachment, fear, or ambition to surrender in prayer.",
        ),
      ) +
      "</li>";
    promptItems +=
      "<li>" +
      escapeHtml(
        summarizeOneLine(
          commentary.seminaVerbi,
          "Look for one resonance with truth beyond your current frame of reference.",
        ),
      ) +
      "</li>";

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

    var linesNode =
      notes.length === 0
        ? '<div class="empty-note">' +
          (hasLlm
            ? "(no line-level marginalia generated)"
            : "LLM output unavailable. Set OPENAI_API_KEY and re-run bun run generate.") +
          "</div>"
        : '<ol class="marginalia-list">' + items + "</ol>";

    return (
      '<div class="marginalia-sections">' +
      '<section class="marginalia-block"><div class="panel-kicker">On This Page</div><ul class="marginalia-links"><li><a class="meta-link" href="#observances">Observances</a></li><li><a class="meta-link" href="#hours-of-prayer">Hours of Prayer</a></li><li><a class="meta-link" href="#reading-first">Reading</a></li><li><a class="meta-link" href="#reading-gospel">Gospel</a></li><li><a class="meta-link" href="#commentary">Commentary</a></li></ul></section>' +
      '<section class="marginalia-block"><div class="panel-kicker">Day Context</div><ul class="marginalia-context">' +
      contextItems +
      "</ul></section>" +
      '<section class="marginalia-block"><div class="panel-kicker">Line Notes</div>' +
      linesNode +
      "</section>" +
      '<section class="marginalia-block"><div class="panel-kicker">Prayer Cues</div><ul class="marginalia-prompts">' +
      promptItems +
      "</ul></section>" +
      "</div>"
    );
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
      '<section class="panel commentary-panel" id="commentary"><header class="panel-header"><div class="panel-kicker">Gloss</div><h2 class="panel-title">Commentary</h2></header>' +
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
      '<section class="panel observances-panel" id="observances"><header class="panel-header"><div class="panel-kicker">Day Office</div><h2 class="panel-title">Observances</h2></header>' +
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

  function renderHoursOfPrayer(artifact) {
    var timeZone = readTimeZone();
    var rows = "";
    for (var i = 0; i < OFFICE_SCHEDULE.length; i += 1) {
      var office = OFFICE_SCHEDULE[i];
      rows +=
        '<li class="hour-row" data-hour-local="' +
        escapeAttr(String(office.hourLocal)) +
        '" data-minute-local="' +
        escapeAttr(String(office.minuteLocal)) +
        '"><div class="hour-name">' +
        escapeHtml(office.label) +
        '</div><div class="hour-time">' +
        escapeHtml(formatLocalClock(office.hourLocal, office.minuteLocal)) +
        '</div><div class="hour-prayer">' +
        escapeHtml(office.prayer) +
        "</div></li>";
    }

    return (
      '<section class="panel hours-panel" id="hours-of-prayer"><header class="panel-header"><div class="panel-kicker">Daily Office</div><h2 class="panel-title">Hours of Prayer</h2><div class="panel-meta">Times shown for ' +
      escapeHtml(timeZone) +
      ".</div></header><ol class=\"hours-list\">" +
      rows +
      "</ol></section>"
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
      renderHoursOfPrayer(artifact) +
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
    enhanceHoursPanel();
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
          enhanceHoursPanel();
        })
        .catch(function () {
          renderError(root, "No day data found for " + targetDate + ".");
        });
    })
    .catch(function (err) {
      renderError(root, err && err.message ? err.message : "Unknown error.");
    });
})();
