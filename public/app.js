(function () {
  "use strict";

  // ============================================================
  // shared utilities
  // ============================================================

  function isIsoDate(value) { return /^\d{4}-\d{2}-\d{2}$/.test(String(value || "")); }
  function pad2(n) { return n < 10 ? "0" + String(n) : String(n); }
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
    var focused = svg.contains(document.activeElement) && document.activeElement.getAttribute("data-station-key");
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
        "aria-pressed": String(h.key === activeKey),
        "aria-label": h.label + " " + pad2(h.hourLocal) + ":" + pad2(h.minuteLocal),
      });
      g.appendChild(svgEl("circle", { "class": "station-glow", "r": "14" }));
      g.appendChild(svgEl("circle", { "class": "station-dot", "r": "7" }));
      g.appendChild(svgEl("text", { "class": "station-label", "y": "-18" }, h.label));
      g.appendChild(svgEl("text", { "class": "station-time", "y": "22" },
        pad2(h.hourLocal) + ":" + pad2(h.minuteLocal)));
      svg.appendChild(g);
      if (h.key === focused) g.focus();
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
    var activeKey = null;

    function refresh() {
      buildHoursArc(svg, hours, currentKey, activeKey || currentKey);
      showHourCard(container, activeKey || currentKey);
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
      currentKey = pickCurrentHourKey(hours, nowMinutesLocal());
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
  function setupCalPeek() {
    var toggle = document.getElementById("cal-toggle");
    var peek = document.getElementById("cal-peek");
    if (!toggle || !peek) return;

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
  // bootstrap
  // ============================================================
  function bootstrap() {
    var date = new URLSearchParams(location.search).get("date");
    if (isIsoDate(date)) {
      var prefix = document.querySelector("main.codex").getAttribute("data-asset-prefix");
      location.replace(prefix + "d/" + date + "/" + location.hash);
      return;
    }
    document.body.classList.add("js-ready");
    enhanceDates();
    enhanceHours();
    setupGutterLinking();
    setupCalPeek();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bootstrap);
  } else {
    bootstrap();
  }
})();
