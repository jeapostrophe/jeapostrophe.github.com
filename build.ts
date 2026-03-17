import { parse as parseYaml } from "jsr:@std/yaml@1";
import { walk, copy } from "jsr:@std/fs@1";
import { ensureDir } from "jsr:@std/fs@1/ensure-dir";
import { join, relative, dirname, basename } from "jsr:@std/path@1";
import MarkdownIt from "npm:markdown-it@14";

const CONTENT_DIR = "content";
const STATIC_DIR = "static";
const SITE_DIR = "_site";
const SITE_TITLE = "Jay McCarthy";
const SECTION_THRESHOLD = 3;

const md = new MarkdownIt({ html: true, typographer: true });

interface FrontMatter {
  title: string;
  date?: string;
  section?: string;
  type?: "article" | "page";
  weight?: number;
  url?: string;
  description?: string;
  format?: string;
  series_order?: number;
}

interface ContentItem {
  meta: FrontMatter;
  body: string;
  html: string;
  srcPath: string;
  slug: string;
  section?: string;
  series?: string;
}

function parseFrontmatter(raw: string): { meta: Record<string, unknown>; body: string } {
  const m = raw.match(/^---\r?\n([\s\S]*?)\r?\n---\r?\n?([\s\S]*)$/);
  if (!m) return { meta: {}, body: raw };
  return { meta: parseYaml(m[1]) as Record<string, unknown>, body: m[2] };
}

function escapeHtml(s: string): string {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function rootRelative(pagePath: string): string {
  const depth = pagePath.split("/").length - 1;
  return depth === 0 ? "." : Array(depth).fill("..").join("/");
}

function template(title: string, content: string, pagePath: string, breadcrumbs?: string): string {
  const pageTitle = title === SITE_TITLE ? title : `${escapeHtml(title)} — ${SITE_TITLE}`;
  const root = rootRelative(pagePath);
  const nav = breadcrumbs
    ? `  <header><nav>${breadcrumbs}</nav></header>\n`
    : "";

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${pageTitle}</title>
  <link rel="stylesheet" href="${root}/theme.css">
  <link rel="stylesheet" href="${root}/style.css">
</head>
<body>
${nav}  <main>
${content}
  </main>
  <footer>
    <p>&copy; Jay McCarthy</p>
  </footer>
</body>
</html>
`;
}

function formatDate(date: string): string {
  const d = new Date(date);
  return d.toLocaleDateString("en-US", {
    year: "numeric", month: "long", day: "numeric",
  });
}

function outPath(item: ContentItem): string {
  const parts = item.srcPath.split("/");
  if (parts.length === 1) {
    return `${item.slug}/index.html`;
  }
  return `${dirname(item.srcPath)}/${item.slug}/index.html`;
}

function itemUrl(item: ContentItem, fromPath?: string): string {
  if (item.meta.url) return item.meta.url;
  const parts = item.srcPath.split("/");
  let abs: string;
  if (parts.length === 1) {
    abs = `${item.slug}/`;
  } else {
    abs = `${dirname(item.srcPath)}/${item.slug}/`;
  }
  if (!fromPath) return abs;
  const root = rootRelative(fromPath);
  return `${root}/${abs}`;
}

async function scanContent(): Promise<ContentItem[]> {
  const items: ContentItem[] = [];

  for await (const entry of walk(CONTENT_DIR, { exts: [".md"] })) {
    if (!entry.isFile) continue;

    const raw = await Deno.readTextFile(entry.path);
    const { meta, body } = parseFrontmatter(raw);
    const fm = meta as FrontMatter;

    const srcPath = relative(CONTENT_DIR, entry.path);
    const name = basename(srcPath, ".md");

    if (name.startsWith("_")) continue;

    const parts = dirname(srcPath).split("/").filter((p) => p !== ".");
    const section = parts[0] || undefined;
    const series = parts.length > 1 ? parts.slice(1).join("/") : undefined;

    let rendered: string;
    if (fm.format === "poetry") {
      const preserved = body.replace(/^( +)/gm, (m) => "&nbsp;".repeat(m.length));
      rendered = `<div class="poetry">\n${md.render(preserved)}\n</div>`;
    } else {
      rendered = md.render(body);
    }

    items.push({
      meta: { type: "article", ...fm },
      body,
      html: rendered,
      srcPath,
      slug: name,
      section,
      series,
    });
  }

  return items;
}

function sortItems(items: ContentItem[]): ContentItem[] {
  const pages = items
    .filter((i) => i.meta.type === "page")
    .sort((a, b) => (a.meta.weight ?? 99) - (b.meta.weight ?? 99));
  const articles = items
    .filter((i) => i.meta.type === "article")
    .sort((a, b) => {
      if (!a.meta.date || !b.meta.date) return 0;
      return new Date(b.meta.date).getTime() - new Date(a.meta.date).getTime();
    });
  return [...pages, ...articles];
}

function renderItemList(items: ContentItem[], fromPath: string): string {
  let html = `<ul class="item-list">\n`;
  for (const item of items) {
    const href = itemUrl(item, fromPath);
    const isExternal = !!item.meta.url;
    const cls = isExternal ? ` class="external"` : "";
    const date = item.meta.date ? `<time>${formatDate(item.meta.date)}</time>` : "";
    const desc = item.meta.description
      ? `<br><span class="description">${escapeHtml(item.meta.description)}</span>`
      : "";
    html += `  <li><a href="${href}"${cls}>${escapeHtml(item.meta.title)}</a>${date ? " " + date : ""}${desc}</li>\n`;
  }
  html += `</ul>`;
  return html;
}

async function buildHomepage(): Promise<void> {
  const homePath = join(CONTENT_DIR, "_home.md");
  const raw = await Deno.readTextFile(homePath);
  const { meta, body } = parseFrontmatter(raw);
  const fm = meta as FrontMatter;
  const rendered = md.render(body);

  const content = `    <h1>${escapeHtml(fm.title || SITE_TITLE)}</h1>
${rendered}`;

  const page = template(SITE_TITLE, content, "index.html");
  await Deno.writeTextFile(join(SITE_DIR, "index.html"), page);
}

function indexedSections(items: ContentItem[]): Set<string> {
  const counts = new Map<string, number>();
  for (const item of items) {
    if (!item.section) continue;
    counts.set(item.section, (counts.get(item.section) || 0) + 1);
  }
  const set = new Set<string>();
  for (const [section, count] of counts) {
    if (count >= SECTION_THRESHOLD) set.add(section);
  }
  return set;
}

function indexedSeries(items: ContentItem[]): Set<string> {
  const counts = new Map<string, number>();
  for (const item of items) {
    if (!item.series || !item.section) continue;
    const key = `${item.section}/${item.series}`;
    counts.set(key, (counts.get(key) || 0) + 1);
  }
  const set = new Set<string>();
  for (const [key, count] of counts) {
    if (count >= 2) set.add(key);
  }
  return set;
}

async function buildPages(items: ContentItem[]): Promise<void> {
  const sections = indexedSections(items);
  const series = indexedSeries(items);

  for (const item of items) {
    if (item.meta.url) continue;

    const op = outPath(item);
    const dest = join(SITE_DIR, op);
    await ensureDir(dirname(dest));

    const root = rootRelative(op);
    const crumbs = [`<a href="${root}/">${SITE_TITLE}</a>`];
    if (item.section) {
      if (sections.has(item.section)) {
        crumbs.push(`<a href="${root}/${item.section}/">${item.section}</a>`);
      } else {
        crumbs.push(item.section);
      }
    }
    if (item.series) {
      const key = `${item.section}/${item.series}`;
      if (series.has(key)) {
        crumbs.push(
          `<a href="${root}/${item.section}/${item.series}/">${item.series}</a>`,
        );
      } else {
        crumbs.push(item.series);
      }
    }

    let content = `    <article>\n      <h1>${escapeHtml(item.meta.title)}</h1>\n`;
    if (item.meta.date) {
      content += `      <time>${formatDate(item.meta.date)}</time>\n`;
    }
    content += item.html;
    content += `\n    </article>`;

    const page = template(item.meta.title, content, op, crumbs.join(" / "));
    await Deno.writeTextFile(dest, page);
  }
}

async function buildSectionIndexes(items: ContentItem[]): Promise<void> {
  const sections = new Map<string, ContentItem[]>();
  for (const item of items) {
    if (!item.section) continue;
    const list = sections.get(item.section) || [];
    list.push(item);
    sections.set(item.section, list);
  }

  for (const [section, sectionItems] of sections) {
    if (sectionItems.length < SECTION_THRESHOLD) continue;

    const sorted = sortItems(sectionItems);
    const label = section.charAt(0).toUpperCase() + section.slice(1);
    const op = `${section}/index.html`;
    const root = rootRelative(op);
    const content = `    <h1>${label}</h1>\n${renderItemList(sorted, op)}`;

    const dest = join(SITE_DIR, op);
    await ensureDir(dirname(dest));
    const crumbs = `<a href="${root}/">${SITE_TITLE}</a> / ${label}`;
    const page = template(label, content, op, crumbs);
    await Deno.writeTextFile(dest, page);
  }
}

async function buildSeriesIndexes(items: ContentItem[]): Promise<void> {
  const seriesMap = new Map<string, ContentItem[]>();
  for (const item of items) {
    if (!item.series || !item.section) continue;
    const key = `${item.section}/${item.series}`;
    const list = seriesMap.get(key) || [];
    list.push(item);
    seriesMap.set(key, list);
  }

  for (const [seriesKey, seriesItems] of seriesMap) {
    if (seriesItems.length < 2) continue;

    const seriesMetaPath = join(CONTENT_DIR, seriesKey, "_series.md");
    let seriesTitle = seriesKey.split("/").pop() || seriesKey;
    let seriesDescription = "";

    try {
      const raw = await Deno.readTextFile(seriesMetaPath);
      const { meta, body } = parseFrontmatter(raw);
      const fm = meta as FrontMatter;
      if (fm.title) seriesTitle = fm.title;
      if (body.trim()) seriesDescription = md.render(body);
    } catch { /* no _series.md, use directory name */ }

    seriesItems.sort((a, b) => {
      const ao = a.meta.series_order ?? 99;
      const bo = b.meta.series_order ?? 99;
      return ao - bo;
    });

    const [section] = seriesKey.split("/");
    const op = `${seriesKey}/index.html`;
    const root = rootRelative(op);

    let content = `    <h1>${escapeHtml(seriesTitle)}</h1>\n`;
    if (seriesDescription) content += seriesDescription;
    content += `<ol>\n`;
    for (const item of seriesItems) {
      content += `  <li><a href="${itemUrl(item, op)}">${escapeHtml(item.meta.title)}</a></li>\n`;
    }
    content += `</ol>`;

    const dest = join(SITE_DIR, op);
    await ensureDir(dirname(dest));
    const sectionCrumb = indexedSections(items).has(section)
      ? `<a href="${root}/${section}/">${section}</a>`
      : section;
    const crumbs = `<a href="${root}/">${SITE_TITLE}</a> / ${sectionCrumb} / ${escapeHtml(seriesTitle)}`;
    const page = template(seriesTitle, content, op, crumbs);
    await Deno.writeTextFile(dest, page);
  }
}

async function copyContentAssets(): Promise<void> {
  for await (const entry of walk(CONTENT_DIR)) {
    if (!entry.isFile) continue;
    if (entry.path.endsWith(".md")) continue;
    const rel = relative(CONTENT_DIR, entry.path);
    const dest = join(SITE_DIR, rel);
    await ensureDir(dirname(dest));
    await Deno.copyFile(entry.path, dest);
  }
}

async function copyStatic(): Promise<void> {
  await copy(STATIC_DIR, SITE_DIR, { overwrite: true });
  await Deno.copyFile("style.css", join(SITE_DIR, "style.css"));
  await Deno.copyFile("theme.css", join(SITE_DIR, "theme.css"));
}

async function clean(): Promise<void> {
  try {
    await Deno.remove(SITE_DIR, { recursive: true });
  } catch { /* doesn't exist yet */ }
}

async function build(): Promise<void> {
  console.log("Building site...");

  await clean();
  await ensureDir(SITE_DIR);

  const items = await scanContent();
  console.log(`Found ${items.length} content items`);

  await copyStatic();
  await copyContentAssets();
  await buildHomepage();
  await buildPages(items);
  await buildSectionIndexes(items);
  await buildSeriesIndexes(items);

  console.log("Done.");
}

build();
