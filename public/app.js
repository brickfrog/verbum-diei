(function () {
  "use strict";

  // ============================================================
  // shared utilities
  // ============================================================

  function escapeHtml(value) {
    return String(value == null ? "" : value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }
  function escapeAttr(value) { return escapeHtml(value); }
  function isIsoDate(value) { return /^\d{4}-\d{2}-\d{2}$/.test(String(value || "")); }
  function pad2(n) { return n < 10 ? "0" + String(n) : String(n); }
  function listOrEmpty(value) { return Array.isArray(value) ? value : []; }
  function kindShort(kind) { return kind === "gospel" ? "G" : "R"; }
  function kindLatin(kind) { return kind === "gospel" ? "Evangelium" : "Lectio"; }
  function kindKicker(kind) { return kind === "gospel" ? "Evangelium" : "Lectio Prima"; }
  function lineId(kind, n) { return String(kind) + "-L" + String(n); }

  function paletteFromSeason(season) {
    var s = String(season || "").toLowerCase();
    if (s.indexOf("lent") !== -1) return "lent";
    if (s.indexOf("easter") !== -1) return "easter";
    if (s.indexOf("advent") !== -1) return "advent";
    if (s.indexOf("christmas") !== -1) return "christmas";
    if (s.indexOf("ordinary") !== -1) return "ordinary";
    return "vespers";
  }

  function hourLatin(key) {
    return ({
      matins: "Ad Matutinum",
      lauds: "Ad Laudes",
      terce: "Ad Tertiam",
      sext: "Ad Sextam",
      none: "Ad Nonam",
      vespers: "Ad Vesperas",
      compline: "Ad Completorium",
    })[String(key)] || "";
  }

  function toRoman(num) {
    var pairs = [["M",1000],["CM",900],["D",500],["CD",400],["C",100],["XC",90],
                 ["L",50],["XL",40],["X",10],["IX",9],["V",5],["IV",4],["I",1]];
    var r = "", n = num;
    for (var i = 0; i < pairs.length; i++) {
      while (n >= pairs[i][1]) { r += pairs[i][0]; n -= pairs[i][1]; }
    }
    return r;
  }

  function romanDate(dateIso) {
    var m = String(dateIso || "").match(/^(\d{4})-(\d{2})-(\d{2})$/);
    if (!m) return dateIso || "";
    return toRoman(Number(m[3])).toLowerCase() +
           " · " + toRoman(Number(m[2])).toLowerCase() +
           " · " + toRoman(Number(m[1]));
  }

  function longDate(dateIso) {
    var m = String(dateIso || "").match(/^(\d{4})-(\d{2})-(\d{2})$/);
    if (!m) return dateIso || "";
    var d = new Date(Date.UTC(Number(m[1]), Number(m[2]) - 1, Number(m[3])));
    try {
      return new Intl.DateTimeFormat("en-US", {
        weekday: "long", year: "numeric", month: "long", day: "numeric",
        timeZone: "UTC",
      }).format(d);
    } catch (_e) {
      return dateIso;
    }
  }

  function hrefLatest(prefix) { return prefix === "" ? "./" : prefix; }
  function hrefArchive(prefix) { return prefix + "archive/"; }
  function hrefDate(prefix, date) { return prefix + "d/" + encodeURIComponent(date) + "/"; }

  function fetchJson(url) {
    return fetch(url, { cache: "no-store" }).then(function (r) {
      if (!r.ok) throw new Error("Request failed: " + r.status + " " + url);
      return r.json();
    });
  }

  function findDateFromPath(pathname) {
    var path = String(pathname || "").replace(/\/+$/, "");
    var m = path.match(/\/d\/(\d{4}-\d{2}-\d{2})$/);
    if (m) return m[1];
    var legacy = path.match(/\/(\d{4}-\d{2}-\d{2})$/);
    if (legacy) return legacy[1];
    return null;
  }

  // ============================================================
  // hours-arc augmentation
  // ============================================================
  // Layout matches Site.purs SVG: viewBox 0 0 760 240, horizon at y=216,
  // arc rx=300 ry=170 centered at (380, 216).
  var ARC = { W: 760, H: 240, cx: 380, cy: 216, rx: 300, ry: 170 };

  function arcAngle(hour) {
    // 0h -> ~178deg, 24h -> ~2deg (slight inset)
    var t = hour / 24;
    return (Math.PI * 0.98) - t * (Math.PI * 0.96);
  }
  function arcPoint(hour, scale) {
    if (scale == null) scale = 1;
    var a = arcAngle(hour);
    return { x: ARC.cx + Math.cos(a) * ARC.rx * scale,
             y: ARC.cy - Math.sin(a) * ARC.ry * scale };
  }

  function svgEl(name, attrs, text) {
    var el = document.createElementNS("http://www.w3.org/2000/svg", name);
    if (attrs) {
      for (var k in attrs) {
        if (Object.prototype.hasOwnProperty.call(attrs, k)) {
          el.setAttribute(k, String(attrs[k]));
        }
      }
    }
    if (text != null) el.textContent = String(text);
    return el;
  }

  function nowMinutesLocal() {
    var d = new Date();
    return d.getHours() * 60 + d.getMinutes();
  }

  function pickCurrentHourKey(hours, nowMin) {
    if (!hours.length) return null;
    var key = hours[0].key;
    for (var i = 0; i < hours.length; i++) {
      var h = hours[i];
      var hm = h.hourLocal * 60 + h.minuteLocal;
      if (hm <= nowMin) key = h.key;
    }
    return key;
  }

  function buildHoursArc(svg, hours, currentKey, activeKey) {
    // wipe any previously-injected dynamic children, leaving the static skeleton
    var dyn = svg.querySelectorAll("[data-arc-dyn]");
    for (var i = 0; i < dyn.length; i++) dyn[i].remove();

    var nowMin = nowMinutesLocal();
    var nowH = nowMin / 60;
    var sun = arcPoint(nowH);

    // station markers
    for (var k = 0; k < hours.length; k++) {
      var h = hours[k];
      var p = arcPoint(h.hourLocal + h.minuteLocal / 60);
      var hm = h.hourLocal * 60 + h.minuteLocal;
      var classes = ["station"];
      var isCurrent = h.key === currentKey;
      if (hm <= nowMin && !isCurrent) classes.push("is-past");
      if (isCurrent) classes.push("is-current");
      if (h.key === activeKey) classes.push("is-active");

      var g = svgEl("g", {
        "class": classes.join(" "),
        "transform": "translate(" + p.x + "," + p.y + ")",
        "data-arc-dyn": "true",
        "data-station-key": h.key,
        "tabindex": "0",
        "role": "button",
        "aria-label": h.label + " " + pad2(h.hourLocal) + ":" + pad2(h.minuteLocal),
      });
      g.appendChild(svgEl("circle", { "class": "station-glow", "r": "14" }));
      g.appendChild(svgEl("circle", { "class": "station-dot", "r": "7" }));
      g.appendChild(svgEl("text", { "class": "station-label", "y": "-18" }, h.label));
      g.appendChild(svgEl("text", { "class": "station-time", "y": "22" },
        pad2(h.hourLocal) + ":" + pad2(h.minuteLocal)));
      svg.appendChild(g);
    }

    // now marker (vertical line + sun)
    var horizonY = ARC.cy;
    var tooClose = false;
    for (var j = 0; j < hours.length; j++) {
      var hp = arcPoint(hours[j].hourLocal + hours[j].minuteLocal / 60);
      if (Math.abs(hp.x - sun.x) < 30 && Math.abs(hp.y - sun.y) < 24) {
        tooClose = true; break;
      }
    }

    var nowLine = svgEl("line", {
      "class": "now-marker",
      "x1": sun.x, "y1": sun.y + 12,
      "x2": sun.x, "y2": horizonY,
      "data-arc-dyn": "true",
    });
    svg.appendChild(nowLine);

    var sunGroup = svgEl("g", {
      "transform": "translate(" + sun.x + "," + sun.y + ")",
      "data-arc-dyn": "true",
    });
    sunGroup.appendChild(svgEl("circle", {
      "r": "9",
      "fill": "var(--gold-light)",
      "stroke": "var(--rubric)",
      "stroke-width": "1.5",
    }));
    sunGroup.appendChild(svgEl("circle", { "r": "3", "fill": "var(--rubric)" }));
    svg.appendChild(sunGroup);

    if (!tooClose) {
      svg.appendChild(svgEl("text", {
        "class": "now-text",
        "x": sun.x, "y": sun.y - 18,
        "data-arc-dyn": "true",
      }, "NOW"));
    }
  }

  function showHourCard(container, key) {
    if (!container) return;
    var cards = container.querySelectorAll(".hour-card");
    for (var i = 0; i < cards.length; i++) {
      var c = cards[i];
      var k = c.getAttribute("data-hour-key");
      var match = k === key;
      c.style.display = match ? "" : "none";
      c.classList.toggle("is-shown", match);
    }
  }

  function applyCurrentHourBadge(container, currentKey) {
    if (!container) return;
    var cards = container.querySelectorAll(".hour-card");
    for (var i = 0; i < cards.length; i++) {
      var c = cards[i];
      var k = c.getAttribute("data-hour-key");
      var isCurrent = k === currentKey;
      c.classList.toggle("is-current", isCurrent);
      var head = c.querySelector(".head");
      if (head) {
        var existing = head.querySelector(".now-badge");
        if (isCurrent && !existing) {
          var badge = document.createElement("span");
          badge.className = "now-badge";
          badge.textContent = "Hora Præsens";
          head.appendChild(badge);
        } else if (!isCurrent && existing) {
          existing.remove();
        }
      }
    }
  }

  function readHoursFromDom() {
    var container = document.getElementById("hours-cards");
    if (!container) return [];
    var cards = container.querySelectorAll(".hour-card");
    var out = [];
    for (var i = 0; i < cards.length; i++) {
      var c = cards[i];
      var key = c.getAttribute("data-hour-key");
      var labelEl = c.querySelector(".label");
      out.push({
        key: key,
        label: labelEl ? labelEl.textContent.trim() : key,
        hourLocal: Number(c.getAttribute("data-hour-local")) || 0,
        minuteLocal: Number(c.getAttribute("data-minute-local")) || 0,
      });
    }
    return out;
  }

  function enhanceHours() {
    var svg = document.querySelector("[data-hours-arc]");
    var container = document.getElementById("hours-cards");
    if (!svg || !container) return;

    var hours = readHoursFromDom();
    if (!hours.length) return;

    var currentKey = pickCurrentHourKey(hours, nowMinutesLocal());
    var activeKey = currentKey;

    function refresh() {
      buildHoursArc(svg, hours, currentKey, activeKey);
      showHourCard(container, activeKey);
      applyCurrentHourBadge(container, currentKey);
    }
    refresh();

    // station click -> swap active
    svg.addEventListener("click", function (ev) {
      var target = ev.target;
      var g = target && target.closest ? target.closest("[data-station-key]") : null;
      if (!g) return;
      var k = g.getAttribute("data-station-key");
      if (!k) return;
      activeKey = k;
      refresh();
    });
    svg.addEventListener("keydown", function (ev) {
      if (ev.key !== "Enter" && ev.key !== " ") return;
      var g = ev.target && ev.target.closest ? ev.target.closest("[data-station-key]") : null;
      if (!g) return;
      ev.preventDefault();
      activeKey = g.getAttribute("data-station-key");
      refresh();
    });

    // tick the now marker every minute so the page stays alive across the day
    setInterval(function () {
      var nk = pickCurrentHourKey(hours, nowMinutesLocal());
      if (nk !== currentKey) {
        currentKey = nk;
        if (activeKey === null) activeKey = currentKey;
      }
      refresh();
    }, 60 * 1000);
  }

  // ============================================================
  // verse <-> gutter-note hover linking + click highlight
  // ============================================================
  var HIGHLIGHT_CLASS = "is-highlighted";
  var ACTIVE_CLASS = "is-active";
  var highlighted = [];
  function clearHighlights() {
    for (var i = 0; i < highlighted.length; i++) {
      highlighted[i].classList.remove(HIGHLIGHT_CLASS);
    }
    highlighted = [];
  }
  function applyHighlights(ids) {
    clearHighlights();
    for (var i = 0; i < ids.length; i++) {
      var el = document.getElementById(ids[i]);
      if (el) { el.classList.add(HIGHLIGHT_CLASS); highlighted.push(el); }
    }
  }

  function setupGutterLinking() {
    var verses = document.querySelectorAll(".verse[data-verse-id]");
    for (var i = 0; i < verses.length; i++) {
      (function (v) {
        var id = v.getAttribute("data-verse-id");
        v.addEventListener("mouseenter", function () {
          v.classList.add(ACTIVE_CLASS);
          var n = document.querySelector('.gutter-note[data-gutter-for="' + id + '"]');
          if (n) n.classList.add(ACTIVE_CLASS);
        });
        v.addEventListener("mouseleave", function () {
          v.classList.remove(ACTIVE_CLASS);
          var n = document.querySelector('.gutter-note[data-gutter-for="' + id + '"]');
          if (n) n.classList.remove(ACTIVE_CLASS);
        });
      })(verses[i]);
    }
    var notes = document.querySelectorAll(".gutter-note[data-gutter-for]");
    for (var j = 0; j < notes.length; j++) {
      (function (n) {
        var id = n.getAttribute("data-gutter-for");
        n.addEventListener("mouseenter", function () {
          n.classList.add(ACTIVE_CLASS);
          var v = document.getElementById(id);
          if (v) v.classList.add(ACTIVE_CLASS);
        });
        n.addEventListener("mouseleave", function () {
          n.classList.remove(ACTIVE_CLASS);
          var v = document.getElementById(id);
          if (v) v.classList.remove(ACTIVE_CLASS);
        });
        n.addEventListener("click", function () {
          var v = document.getElementById(id);
          if (v) v.scrollIntoView({ block: "center", behavior: "smooth" });
        });
      })(notes[j]);
    }
  }

  // commentary note-ref click -> highlight target verses
  document.addEventListener("click", function (ev) {
    var t = ev.target;
    if (!t || typeof t.closest !== "function") return;
    var link = t.closest("a.note-ref");
    if (!link) return;
    var raw = link.getAttribute("data-hl");
    if (!raw) return;
    var ids = raw.split(/\s+/).filter(Boolean);
    if (ids.length > 0) applyHighlights(ids);
  }, true);

  // ============================================================
  // long-date formatting (replace ISO with friendly string)
  // ============================================================
  function enhanceDates() {
    var nodes = document.querySelectorAll(".date-long[data-iso]");
    for (var i = 0; i < nodes.length; i++) {
      var iso = nodes[i].getAttribute("data-iso");
      if (isIsoDate(iso)) {
        nodes[i].textContent = longDate(iso);
      }
    }
  }

  // ============================================================
  // calendar peek (date jumper)
  // ============================================================
  function setupCalPeek(assetPrefix, currentDate, archiveDates) {
    var toggle = document.getElementById("cal-toggle");
    var peek = document.getElementById("cal-peek");
    var list = document.getElementById("cal-peek-list");
    if (!toggle || !peek) return;

    if (list) {
      var recent = listOrEmpty(archiveDates).slice(0, 14);
      var html = "";
      for (var i = 0; i < recent.length; i++) {
        var d = recent[i];
        var isCurrent = d === currentDate;
        html += '<li><a href="' + escapeAttr(hrefDate(assetPrefix, d)) +
                '" class="' + (isCurrent ? "is-current" : "") + '">' +
                '<span class="d">' + escapeHtml(d.slice(5)) + '</span>' +
                '<span>' + escapeHtml(d) + '</span></a></li>';
      }
      if (!html) {
        html = '<li><span class="d">—</span><span>No days</span></li>';
      }
      list.innerHTML = html;
    }

    function close() {
      peek.classList.add("is-closed");
      toggle.setAttribute("aria-expanded", "false");
    }
    function open() {
      peek.classList.remove("is-closed");
      toggle.setAttribute("aria-expanded", "true");
    }
    toggle.addEventListener("click", function (e) {
      e.stopPropagation();
      if (peek.classList.contains("is-closed")) open(); else close();
    });
    document.addEventListener("click", function (e) {
      if (peek.classList.contains("is-closed")) return;
      if (peek.contains(e.target) || toggle.contains(e.target)) return;
      close();
    });
    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") close();
    });
  }

  // ============================================================
  // SPA-shell rendering (older permalink pages)
  // ============================================================
  function renderShellDay(root, artifact, _dates, assetPrefix) {
    var palette = paletteFromSeason(((artifact.observances || {}).meta || {}).season || "");
    document.documentElement.setAttribute("data-palette", palette);

    var first = null, gospel = null;
    var readings = listOrEmpty(artifact.readings);
    for (var i = 0; i < readings.length; i++) {
      var r = readings[i] || {};
      if (r.kind === "gospel") gospel = r;
      else if (!first) first = r;
    }
    var marginalia = listOrEmpty(artifact.marginalia);
    var meta = (artifact.observances || {}).meta || {};
    var celebrations = listOrEmpty((artifact.observances || {}).celebrations);
    var commentary = artifact.commentary || {};
    var hasLlm = !!artifact.llm && Array.isArray(artifact.llm.calls) && artifact.llm.calls.length > 0;

    function notesByLineIdx(reading, idx) {
      var kindNotes = marginalia.filter(function (n) { return n.readingKind === reading.kind; });
      for (var i = 0; i < kindNotes.length; i++) {
        if (Array.isArray(kindNotes[i].lines) && kindNotes[i].lines.indexOf(idx) !== -1) {
          return kindNotes[i];
        }
      }
      return null;
    }

    function readingHtml(reading, romanIdx) {
      if (!reading) return "";
      var lines = listOrEmpty(reading.lines);
      var lineRefs = listOrEmpty(reading.lineRefs);
      var rows = "";
      for (var i = 0; i < lines.length; i++) {
        var n = i + 1;
        var id = lineId(reading.kind, n);
        var label = String(lineRefs[i] || n);
        var note = notesByLineIdx(reading, n);
        var hasMargin = note ? "true" : "false";
        var verse =
          '<p class="verse" id="' + escapeAttr(id) +
          '" data-verse-id="' + escapeAttr(id) +
          '" data-has-margin="' + hasMargin + '"' +
          ' data-line-ref="' + escapeAttr(label) + '">' +
          '<span class="vn">' + escapeHtml(label) + '</span>' +
          '<span class="vt">' + escapeHtml(lines[i]) + '</span></p>';
        var gutter = note
          ? '<aside class="gutter-note" data-gutter-for="' + escapeAttr(id) + '">' +
            '<span class="manicule">☞</span>' +
            '<span class="ref-tag">' + kindShort(reading.kind) + " " + escapeHtml(label) + '</span>' +
            '<div>' + escapeHtml(note.text || "") + '</div></aside>'
          : "";
        rows += '<div class="verse-row">' + verse + gutter + "</div>";
      }
      var translation = reading.translation || {};
      var panelId = reading.kind === "gospel" ? "reading-gospel" : "reading-first";
      return '<section class="section reading" id="' + panelId + '">' +
        '<h2 class="rubric-heading">' +
          '<span class="num">' + romanIdx + '</span>' +
          '<span class="title-main">' + kindLatin(reading.kind) + '</span>' +
          '<span class="latin">' + escapeHtml(reading.reference || "") + '</span>' +
        '</h2>' +
        '<div class="reading-head">' +
          '<div class="kicker">' + kindKicker(reading.kind) + '</div>' +
          '<h3 class="heading">' + escapeHtml(reading.heading || "") + '</h3>' +
          '<div class="ref">' + escapeHtml((reading.reference || "") + " · " + (translation.name || "")) + '</div>' +
        '</div>' +
        '<div class="scripture with-dropcap">' + rows + '</div>' +
        '</section>';
    }

    function commentNoteList(kind, notes) {
      var arr = listOrEmpty(notes);
      if (!arr.length) {
        return '<div class="empty-note">' + (hasLlm ? "(no notes)" : "LLM output unavailable.") + '</div>';
      }
      var items = "";
      for (var i = 0; i < arr.length; i++) {
        var note = arr[i] || {};
        var lines = listOrEmpty(note.lines);
        var hl = lines.map(function (n) { return lineId(kind, n); }).join(" ");
        var label = kindShort(kind) + " " + lines.join(",");
        items += '<li><a class="note-ref" href="#' + (lines.length ? lineId(kind, lines[0]) : "") +
                 '" data-hl="' + escapeAttr(hl) + '">' + escapeHtml(label) + '</a>' +
                 '<span class="note-text"> ' + escapeHtml(note.text || "") + '</span></li>';
      }
      return '<ul>' + items + '</ul>';
    }

    function paragraphs(text) {
      var t = String(text || "").trim();
      if (!t) return "";
      var parts = t.split(/\n\n+/);
      var out = "";
      for (var i = 0; i < parts.length; i++) out += '<p>' + escapeHtml(parts[i]) + '</p>';
      return out;
    }

    var observancesHtml =
      '<section class="section" id="observances">' +
        '<h2 class="rubric-heading">' +
          '<span class="num">I</span>' +
          '<span class="title-main">Observantiæ</span>' +
          '<span class="latin">Dies Hodierna</span>' +
        '</h2>' +
        '<div class="observance-row">' +
          '<div class="observance-cell"><div class="lab">Tempus</div><div class="val">' + escapeHtml(meta.season || "—") + '</div></div>' +
          '<div class="observance-cell"><div class="lab">Cyclus</div><div class="val">' + escapeHtml(meta.cycle || "—") + '</div></div>' +
          '<div class="observance-cell"><div class="lab">Psalterium</div><div class="val">' + escapeHtml(meta.psalterWeek || "—") + '</div></div>' +
        '</div>' +
        celebrations.map(function (c) {
          return '<div class="celebration">' +
            '<svg class="saint-mark" viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="1.4" aria-hidden="true">' +
              '<path d="M16 3 L16 29 M9 11 L23 11 M11 22 L21 22"/>' +
              '<circle cx="16" cy="7" r="2.4" fill="currentColor" stroke="none"/>' +
            '</svg>' +
            '<span class="rank">' + escapeHtml(c.rank || "—") + '</span>' +
            '<span class="name">' + escapeHtml(c.name || "") + '</span>' +
          '</div>';
        }).join("") +
      '</section>';

    var hours = listOrEmpty(artifact.hoursOfPrayer);
    if (!hours.length) {
      hours = [
        { key: "matins", label: "Matins", hourLocal: 0, minuteLocal: 0, prayer: "Lord, open my lips, and my mouth shall declare your praise.", source: "fallback" },
        { key: "lauds", label: "Lauds", hourLocal: 6, minuteLocal: 0, prayer: "Blessed are you, Lord, in the light of the new day.", source: "fallback" },
        { key: "terce", label: "Terce", hourLocal: 9, minuteLocal: 0, prayer: "Come, Holy Spirit, and lighten our work in truth.", source: "fallback" },
        { key: "sext", label: "Sext", hourLocal: 12, minuteLocal: 0, prayer: "God, come to my assistance. Lord, make haste to help me.", source: "fallback" },
        { key: "none", label: "Nones", hourLocal: 15, minuteLocal: 0, prayer: "Stay with us, Lord, in the heat and trial of this day.", source: "fallback" },
        { key: "vespers", label: "Vespers", hourLocal: 18, minuteLocal: 0, prayer: "Let my prayer rise before you like incense this evening.", source: "fallback" },
        { key: "compline", label: "Compline", hourLocal: 21, minuteLocal: 0, prayer: "Into your hands, Lord, I commend my spirit.", source: "fallback" },
      ];
    }
    var hoursCards = "";
    for (var hi = 0; hi < hours.length; hi++) {
      var h = hours[hi];
      hoursCards +=
        '<div class="hour-card" data-hour-key="' + escapeAttr(h.key) + '"' +
        ' data-hour-local="' + escapeAttr(h.hourLocal) + '"' +
        ' data-minute-local="' + escapeAttr(h.minuteLocal) + '"' +
        ' data-hour-source="' + escapeAttr(h.source || "fallback") + '">' +
          '<div class="head">' +
            '<span class="label">' + escapeHtml(h.label) + '</span>' +
            '<span class="latin-name">' + escapeHtml(hourLatin(h.key)) + '</span>' +
            '<span class="time">' + pad2(h.hourLocal) + ":" + pad2(h.minuteLocal) + '</span>' +
          '</div>' +
          '<div class="prayer">' + escapeHtml(h.prayer || "") + '</div>' +
        '</div>';
    }
    var hoursHtml =
      '<section class="section" id="hours-of-prayer">' +
        '<h2 class="rubric-heading">' +
          '<span class="num">II</span>' +
          '<span class="title-main">Horæ Canonicæ</span>' +
          '<span class="latin">Officium Divinum</span>' +
        '</h2>' +
        '<div class="section-meta">Seven stations · the sun above traces the day.</div>' +
        '<div class="hours-wrap">' +
          '<svg class="clock-arc" viewBox="0 0 760 240" preserveAspectRatio="xMidYMid meet" data-hours-arc="true" aria-hidden="true">' +
            '<line class="horizon" x1="15" y1="216" x2="745" y2="216"/>' +
            '<path class="arc-line" d="M 80 216 A 300 170 0 0 1 680 216"/>' +
            '<path class="arc-day" d="M 80 216 A 300 170 0 0 1 680 216"/>' +
          '</svg>' +
        '</div>' +
        '<div id="hours-cards" data-hours-list="true">' + hoursCards + '</div>' +
      '</section>';

    var divider = '<div class="divider-rule" aria-hidden="true">' +
      '<span class="line"></span><span class="glyph">✠ ❦ ✠</span><span class="line"></span></div>';

    var commentaryHtml =
      '<section class="section" id="commentary">' +
        '<h2 class="rubric-heading">' +
          '<span class="num">V</span>' +
          '<span class="title-main">Glossa</span>' +
          '<span class="latin">Commentarium</span>' +
        '</h2>' +
        (commentary.synthesis ? '<div class="synthesis"><span class="lead">Sententia Doctrinalis</span>' +
          escapeHtml(commentary.synthesis) + '</div>' : "") +
        ((listOrEmpty(commentary.reading).length || listOrEmpty(commentary.gospel).length) ?
          '<div class="commentary-cols">' +
            '<section class="commentary-col"><h4>In Lectionem</h4>' + commentNoteList("first", commentary.reading) + '</section>' +
            '<section class="commentary-col"><h4>In Evangelium</h4>' + commentNoteList("gospel", commentary.gospel) + '</section>' +
          '</div>' : "") +
        ((commentary.excursus || hasLlm) ? '<section class="long-prose with-dropcap">' +
          '<h4>Lectio Heterodoxa</h4>' +
          (paragraphs(commentary.excursus) || '<p class="empty-note">(no heterodox reading generated)</p>') +
          '</section>' : "") +
        ((commentary.seminaVerbi || hasLlm) ? '<section class="long-prose">' +
          '<h4>Semina Verbi</h4>' +
          (paragraphs(commentary.seminaVerbi) || '<p class="empty-note">(no semina verbi generated)</p>') +
          '</section>' : "") +
      '</section>';

    var firstTrans = (first && first.translation) || { id: "dra", name: "Douay-Rheims 1899" };
    var translationHrefValue = firstTrans.id === "dra" ? "https://www.gutenberg.org/ebooks/8300" : "https://www.gutenberg.org/";
    var footerHtml =
      '<footer class="codex-footer">' +
        '<div>Scripturæ textus: <a href="' + escapeAttr(translationHrefValue) + '">' +
          escapeHtml(firstTrans.name + " · Public Domain") + '</a></div>' +
        '<div>Calendarium per <a href="https://github.com/romcal/romcal">romcal</a> · ' +
          'Horarum textus per <a href="https://github.com/DavidLara/breviarium">breviarium</a></div>' +
        '<div>Marginalia et glossæ a machina linguistica generatæ — ad meditationem, non ad doctrinam.</div>' +
        '<div>Source: <a href="https://github.com/brickfrog/verbum-diei">github.com/brickfrog/verbum-diei</a></div>' +
        '<div class="explicit">Explicit Liber · Deo Gratias</div>' +
      '</footer>';

    var pillsHtml = "";
    if (artifact.source && artifact.source.itemUrl) {
      pillsHtml += '<a class="nav-pill" href="' + escapeAttr(artifact.source.itemUrl) + '" rel="noreferrer">Vatican</a>';
    }
    pillsHtml += '<a class="nav-pill" href="' + escapeAttr(hrefArchive(assetPrefix)) + '">Archive</a>';
    pillsHtml += '<a class="nav-pill" href="' + escapeAttr(hrefLatest(assetPrefix)) + '">Latest</a>';

    var fleuron =
      '<svg class="fleuron {{POS}}" viewBox="0 0 56 56" fill="none" stroke="currentColor" stroke-width="1.2" aria-hidden="true">' +
        '<path d="M4 4 Q 14 4, 14 14 Q 14 24, 24 24"/>' +
        '<path d="M4 4 Q 4 14, 14 14"/>' +
        '<circle cx="14" cy="14" r="2.2" fill="currentColor" stroke="none"/>' +
        '<path d="M28 8 Q 22 14, 28 22 Q 34 14, 28 8 Z" fill="currentColor" opacity="0.6" stroke="none"/>' +
        '<path d="M8 28 Q 14 22, 22 28 Q 14 34, 8 28 Z" fill="currentColor" opacity="0.6" stroke="none"/>' +
        '<path d="M28 28 m -3 0 a 3 3 0 1 0 6 0 a 3 3 0 1 0 -6 0" fill="currentColor" stroke="none"/>' +
      '</svg>';

    // Top-level shell uses #app-root which has class "codex"
    root.className = "codex";
    root.innerHTML =
      '<article class="codex-page">' +
        fleuron.replace("{{POS}}", "tl") +
        fleuron.replace("{{POS}}", "tr") +
        fleuron.replace("{{POS}}", "bl") +
        fleuron.replace("{{POS}}", "br") +
        '<header class="colophon">' +
          '<div class="colophon-left">' +
            '<span>Codex Verbi · Folio</span>' +
            '<span style="color:var(--rubric)">Scriptura · Oratio · Glossa</span>' +
          '</div>' +
          '<div class="wordmark">' +
            '<h1 class="v-d">Verbum Diei</h1>' +
            '<div class="latin">Lectionarium Cottidianum</div>' +
          '</div>' +
          '<div class="colophon-right">' +
            '<span class="date-long" data-iso="' + escapeAttr(artifact.date || "") + '">' + escapeHtml(artifact.date || "") + '</span>' +
            '<span class="date-roman">' + escapeHtml(romanDate(artifact.date)) + '</span>' +
            '<span class="nav-pills">' + pillsHtml + '</span>' +
          '</div>' +
        '</header>' +
        '<div class="codex-grid placement-gutter">' +
          '<div class="center-column">' +
            observancesHtml +
            hoursHtml +
            divider +
            readingHtml(first, "III") +
            readingHtml(gospel, "IV") +
            commentaryHtml +
            footerHtml +
          '</div>' +
          '<aside class="gutter-rail-col">' +
            '<div class="gutter-rail">' +
              '<h4>In Hac Pagina</h4>' +
              '<ul>' +
                '<li><a href="#observances">Observantiæ</a></li>' +
                '<li><a href="#hours-of-prayer">Horæ Canonicæ</a></li>' +
                '<li><a href="#reading-first">Lectio</a></li>' +
                '<li><a href="#reading-gospel">Evangelium</a></li>' +
                '<li><a href="#commentary">Glossa</a></li>' +
              '</ul>' +
              (commentary.synthesis ?
                '<h4>Sententia</h4><p class="day-brief">' + escapeHtml(commentary.synthesis) + '</p>' : "") +
            '</div>' +
          '</aside>' +
        '</div>' +
      '</article>';

    document.title = "Verbum Diei · " + (artifact.date || "");
  }

  function renderShellArchive(root, dates, assetPrefix) {
    document.documentElement.setAttribute("data-palette", "vespers");
    var items = "";
    for (var i = 0; i < dates.length; i++) {
      items += '<li class="archive-item"><a href="' + escapeAttr(hrefDate(assetPrefix, dates[i])) +
               '">' + escapeHtml(dates[i]) + '</a></li>';
    }
    if (!items) items = '<div class="empty-note">No generated days yet.</div>';

    root.className = "codex";
    root.innerHTML =
      '<article class="codex-page">' +
        '<header class="colophon">' +
          '<div class="colophon-left"><span>Chronicon · Codex Dierum</span></div>' +
          '<div class="wordmark">' +
            '<h1 class="v-d">Archivum</h1>' +
            '<div class="latin">Codex Dierum</div>' +
          '</div>' +
          '<div class="colophon-right">' +
            '<span class="nav-pills">' +
              '<a class="nav-pill" href="' + escapeAttr(hrefLatest(assetPrefix)) + '">Latest</a>' +
            '</span>' +
          '</div>' +
        '</header>' +
        '<div class="codex-grid" style="grid-template-columns:1fr">' +
          '<div class="center-column">' +
            '<section class="section">' +
              '<h2 class="rubric-heading">' +
                '<span class="num">I</span>' +
                '<span class="title-main">Dies</span>' +
                '<span class="latin">Omnia</span>' +
              '</h2>' +
              (dates.length ? '<ul class="archive-list">' + items + '</ul>' : items) +
            '</section>' +
            '<footer class="codex-footer">' +
              '<div>Generated daily.</div>' +
              '<div class="explicit">Codex Dierum · Deo Gratias</div>' +
            '</footer>' +
          '</div>' +
        '</div>' +
      '</article>';

    document.title = "Verbum Diei · Archive";
  }

  function renderShellError(root, message) {
    root.className = "codex";
    root.innerHTML =
      '<article class="codex-page"><div class="codex-grid" style="grid-template-columns:1fr">' +
      '<div class="center-column"><section class="section">' +
      '<h2 class="rubric-heading"><span class="num">!</span><span class="title-main">Error</span></h2>' +
      '<div class="empty-note">' + escapeHtml(message) + '</div></section></div></div></article>';
  }

  // ============================================================
  // bootstrap
  // ============================================================
  function readAssetPrefixFromMain() {
    var main = document.querySelector("main.codex, #app-root");
    return main ? (main.getAttribute("data-asset-prefix") || "") : "";
  }

  function readCurrentDateFromDom() {
    var node = document.querySelector(".date-long[data-iso]");
    if (node) {
      var iso = node.getAttribute("data-iso");
      if (isIsoDate(iso)) return iso;
    }
    return null;
  }

  function bootstrap() {
    var shellRoot = document.getElementById("app-root");
    var assetPrefix = readAssetPrefixFromMain();

    if (shellRoot) {
      // SPA shell page (older permalinks): fetch + render
      var defaultView = shellRoot.getAttribute("data-default-view") || "latest";
      fetchJson(assetPrefix + "data/archive.json")
        .then(function (payload) {
          var dates = listOrEmpty(payload && payload.dates).filter(isIsoDate).sort().reverse();
          var params = new URLSearchParams(location.search);
          var dq = params.get("date");
          var dp = findDateFromPath(location.pathname);
          var target = isIsoDate(dq) ? dq
                      : isIsoDate(dp) ? dp
                      : (defaultView !== "archive" && dates.length ? dates[0] : null);
          if (!target) {
            renderShellArchive(shellRoot, dates, assetPrefix);
            postRender(assetPrefix, null, dates);
            return;
          }
          return fetchJson(assetPrefix + "data/" + target + ".json")
            .then(function (artifact) {
              renderShellDay(shellRoot, artifact || {}, dates, assetPrefix);
              postRender(assetPrefix, artifact && artifact.date, dates);
            })
            .catch(function () {
              renderShellError(shellRoot, "No day data found for " + target + ".");
            });
        })
        .catch(function (err) {
          renderShellError(shellRoot, err && err.message ? err.message : "Unknown error.");
        });
    } else {
      // Static SSR page — just enhance.
      var currentDate = readCurrentDateFromDom();
      fetchJson(assetPrefix + "data/archive.json")
        .then(function (payload) {
          var dates = listOrEmpty(payload && payload.dates).filter(isIsoDate).sort().reverse();
          postRender(assetPrefix, currentDate, dates);
        })
        .catch(function () {
          postRender(assetPrefix, currentDate, []);
        });
    }
  }

  function postRender(assetPrefix, currentDate, archiveDates) {
    document.body.classList.add("js-ready");
    enhanceDates();
    enhanceHours();
    setupGutterLinking();
    setupCalPeek(assetPrefix, currentDate, archiveDates);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bootstrap);
  } else {
    bootstrap();
  }
})();
