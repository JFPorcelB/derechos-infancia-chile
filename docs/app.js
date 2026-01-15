// docs/app.js

const INDICATORS_ORDER = [
  "casen_pov_inc",
  "casen_pov_mdim",
  "casen_attend",
  "casen_noatt"
];

let DATA = null;
let selectedRegionCode = 4; // Coquimbo (default)

// Helpers
function fmtPct(x) {
  return `${Number(x).toFixed(1)}%`;
}

function fmtDelta(delta) {
  const d = Number(delta);
  const arrow = d >= 0 ? "↑" : "↓";
  return `${arrow} ${Math.abs(d).toFixed(1)} pp vs Chile`;
}

function getRegionBlock(regionCode) {
  if (!DATA?.regions) return null;
  return DATA.regions.find(r => Number(r.region_code) === Number(regionCode)) || null;
}

function renderCards(regionCode) {
  const grid = document.getElementById("cardsGrid");
  if (!grid) return;

  const regionBlock = getRegionBlock(regionCode);
  if (!regionBlock) {
    grid.innerHTML = `<div style="padding:16px;">No encuentro datos para región ${regionCode}.</div>`;
    return;
  }

  // Mantener solo los 4 indicadores (y en orden)
  const kpiMap = new Map(regionBlock.kpis.map(k => [k.indicator_id, k]));
  const kpis = INDICATORS_ORDER
    .map(id => kpiMap.get(id))
    .filter(Boolean);

  grid.innerHTML = kpis.map(k => {
    // Si no existe la imagen, igual mostramos card sin romper
    const figHtml = k.fig
      ? `<img class="kpi-fig" src="${k.fig}" alt="${k.indicator_label}"/>`
      : `<div class="kpi-fig kpi-fig-placeholder"></div>`;

    return `
      <div class="kpi-card">
        <div class="kpi-title">${k.indicator_label}</div>

        <div class="kpi-fig-wrap">
          ${figHtml}
        </div>

        <div class="kpi-values">
          <div class="kpi-row">
            <span class="kpi-label">Chile</span>
            <span class="kpi-val">${fmtPct(k.chile)}</span>
          </div>
          <div class="kpi-row">
            <span class="kpi-label">${regionBlock.region_name}</span>
            <span class="kpi-val">${fmtPct(k.region)}</span>
          </div>
        </div>

        <div class="kpi-delta">Δ <strong>${fmtDelta(k.delta_pp)}</strong></div>

        <div class="kpi-foot">
          <span class="chip">${k.source}</span>
          <span class="chip">${k.universe}</span>
        </div>
      </div>
    `;
  }).join("");
}

async function init() {
  // Ojo: esto requiere abrir el sitio por http (no file://)
  const res = await fetch("data/kpis.json");
  DATA = await res.json();

  renderCards(selectedRegionCode);
}

window.addEventListener("DOMContentLoaded", init);

// (Después lo conectamos al mapa)
// Ejemplo futuro: window.setRegion = (code) => { selectedRegionCode = code; renderCards(code); };
