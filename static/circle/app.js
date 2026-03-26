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

  window.addEventListener("hashchange", route);
  route();
})();
