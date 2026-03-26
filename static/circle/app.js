(function () {
  const currentYear = 1;
  const currentBook = 1;

  function activate(selector, cls, fn) {
    document.querySelectorAll(selector).forEach(el => el.classList.toggle(cls, fn(el)));
  }

  // Show/hide nav segments based on context
  function updateNav(opts) {
    // opts: { main, cycle, year }
    // main: which main tab is active
    // cycle: which cycle sub-tab (or null)
    // year: which year number for pills (or null)

    // Main tabs
    activate(".main-tab", "active", t => t.dataset.mainTab === opts.main);

    // Cycle tabs segment: visible when readings is active
    const showCycle = opts.main === "readings";
    document.querySelector(".nav-cycle-tabs").classList.toggle("active", showCycle);
    if (showCycle) {
      activate(".cycle-tab", "active", t => t.dataset.cycleTab === opts.cycle);
    }

    // Arc pills: visible when cycle-about is the active cycle tab
    document.querySelector(".nav-arc-pills").classList.toggle("active", showCycle && opts.cycle === "cycle-about");
    if (showCycle && opts.cycle === "cycle-about") {
      activate(".arc-pill", "active", p => parseInt(p.dataset.arc) === (opts.arc ?? 0));
    }

    // Year pill segments: show only the matching year
    document.querySelectorAll(".nav-pills").forEach(seg => {
      const yr = parseInt(seg.dataset.forYear);
      seg.classList.toggle("active", showCycle && yr === opts.year);
    });
    if (opts.year) {
      activate(".book-pill[data-year='" + opts.year + "']", "active",
        p => parseInt(p.dataset.book) === (opts.book ?? 0));
    }
  }

  function showMainPanel(panelId) {
    document.querySelectorAll(".main-panel, #readings").forEach(p => p.classList.remove("active"));
    const el = document.getElementById(panelId);
    if (el) el.classList.add("active");
  }

  function showCyclePanel(panelId) {
    showMainPanel("readings");
    document.querySelectorAll(".cycle-panel").forEach(p => p.classList.remove("active"));
    const el = document.getElementById(panelId);
    if (el) el.classList.add("active");
  }

  function showBook(yearNum, bookNum) {
    const yearPanel = document.getElementById("cycle-year-" + yearNum);
    if (!yearPanel) return;
    yearPanel.querySelectorAll(".book-card").forEach(c => {
      c.classList.toggle("active", parseInt(c.dataset.book) === bookNum);
    });
    yearPanel.querySelectorAll(".book-next").forEach(c => {
      c.classList.toggle("active", parseInt(c.dataset.book) === bookNum);
    });
  }

  function showArc(arcNum) {
    const cycleAbout = document.getElementById("cycle-about");
    if (!cycleAbout) return;
    cycleAbout.querySelectorAll(".arc-card").forEach(c => {
      c.classList.toggle("active", parseInt(c.dataset.arc) === arcNum);
    });
    cycleAbout.querySelectorAll(".arc-next").forEach(c => {
      c.classList.toggle("active", parseInt(c.dataset.arc) === arcNum);
    });
  }

  function route() {
    const hash = location.hash.slice(1) || "about";

    // cycle-year-N-book-M
    const bookMatch = hash.match(/^cycle-year-(\d+)-book-(\d+)$/);
    if (bookMatch) {
      const yr = parseInt(bookMatch[1]);
      const bk = parseInt(bookMatch[2]);
      showCyclePanel("cycle-year-" + yr);
      showBook(yr, bk);
      updateNav({ main: "readings", cycle: "cycle-year-" + yr, year: yr, book: bk });
      return;
    }

    // cycle-year-N-about
    const yearAboutMatch = hash.match(/^cycle-year-(\d+)-about$/);
    if (yearAboutMatch) {
      const yr = parseInt(yearAboutMatch[1]);
      showCyclePanel("cycle-year-" + yr);
      showBook(yr, 0);
      updateNav({ main: "readings", cycle: "cycle-year-" + yr, year: yr, book: 0 });
      return;
    }

    // cycle-year-N
    const yearMatch = hash.match(/^cycle-year-(\d+)$/);
    if (yearMatch) {
      const yr = parseInt(yearMatch[1]);
      showCyclePanel("cycle-year-" + yr);
      showBook(yr, 0);
      updateNav({ main: "readings", cycle: "cycle-year-" + yr, year: yr, book: 0 });
      return;
    }

    // cycle-arc-N
    const arcMatch = hash.match(/^cycle-arc-(\d+)$/);
    if (arcMatch) {
      const arcNum = parseInt(arcMatch[1]);
      showCyclePanel("cycle-about");
      showArc(arcNum);
      updateNav({ main: "readings", cycle: "cycle-about", arc: arcNum });
      return;
    }

    // cycle-about
    if (hash === "cycle-about") {
      showCyclePanel("cycle-about");
      showArc(0);
      updateNav({ main: "readings", cycle: "cycle-about", arc: 0 });
      return;
    }

    // readings
    if (hash === "readings") {
      showCyclePanel("cycle-about");
      showArc(0);
      updateNav({ main: "readings", cycle: "cycle-about", arc: 0 });
      return;
    }

    // Main panel
    showMainPanel(hash);
    updateNav({ main: hash });
  }

  // --- Private tab decryption ---
  const privatePanel = document.getElementById("private");
  if (privatePanel) {
    const STORAGE_KEY = "circle-private-pass";

    function fromBase64(b64) {
      const bin = atob(b64);
      const arr = new Uint8Array(bin.length);
      for (let i = 0; i < bin.length; i++) arr[i] = bin.charCodeAt(i);
      return arr;
    }

    async function tryDecrypt(password) {
      const salt = fromBase64(privatePanel.dataset.salt);
      const iv = fromBase64(privatePanel.dataset.iv);
      const ct = fromBase64(privatePanel.dataset.ct);

      const keyMaterial = await crypto.subtle.importKey(
        "raw", new TextEncoder().encode(password), "PBKDF2", false, ["deriveKey"]
      );
      const key = await crypto.subtle.deriveKey(
        { name: "PBKDF2", salt: salt, iterations: 600000, hash: "SHA-256" },
        keyMaterial,
        { name: "AES-GCM", length: 256 },
        false,
        ["decrypt"]
      );
      const plain = await crypto.subtle.decrypt({ name: "AES-GCM", iv: iv }, key, ct);
      return new TextDecoder().decode(plain);
    }

    function showContent(html) {
      privatePanel.querySelector(".private-lock").hidden = true;
      const content = privatePanel.querySelector(".private-content");
      content.innerHTML = html;
      content.hidden = false;
    }

    const form = privatePanel.querySelector(".private-form");
    const input = privatePanel.querySelector(".private-input");
    const error = privatePanel.querySelector(".private-error");
    const reveal = privatePanel.querySelector(".private-reveal");

    reveal.addEventListener("click", function () {
      const show = input.type === "password";
      input.type = show ? "text" : "password";
      reveal.querySelector(".eye-open").style.display = show ? "none" : "block";
      reveal.querySelector(".eye-closed").style.display = show ? "block" : "none";
    });

    form.addEventListener("submit", async function (e) {
      e.preventDefault();
      const pass = input.value;
      if (!pass) return;
      error.hidden = true;
      try {
        const html = await tryDecrypt(pass);
        localStorage.setItem(STORAGE_KEY, pass);
        showContent(html);
      } catch (err) {
        console.error("Decryption failed:", err);
        error.hidden = false;
        input.select();
      }
    });

    // Auto-unlock from localStorage
    const saved = localStorage.getItem(STORAGE_KEY);
    if (saved) {
      tryDecrypt(saved).then(showContent).catch(() => localStorage.removeItem(STORAGE_KEY));
    }
  }

  // Clicking logo/title when already on #about should scroll to top, not to the #about anchor
  document.querySelectorAll('.logo-link, .title-link').forEach(a => {
    a.addEventListener('click', function(e) {
      if (!location.hash || location.hash === '#about') {
        e.preventDefault();
        window.scrollTo(0, 0);
      }
    });
  });

  window.addEventListener("hashchange", route);
  route();
})();
