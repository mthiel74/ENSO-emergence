"""
Generate five ENSO educational illustration PNGs using OpenAI gpt-image-2.
Run: python generate_enso_figures.py
Requires: OPENAI_API_KEY in environment, openai Python SDK installed.
"""

import base64
import os
import sys
import time
from pathlib import Path

from openai import OpenAI

# Default to the directory containing this script (the same
# docs/images/figures-generated/ the rest of the pipeline reads from).
# Override with $OUT_DIR if you want to render somewhere else.
OUTPUT_DIR = Path(os.environ.get("OUT_DIR", Path(__file__).resolve().parent))
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

client = OpenAI()

FIGURES = [
    (
        "nino34_region_map.png",
        (
            "Educational scientific illustration of the tropical Pacific Ocean for a climate science article. "
            "World map projection centered on the Pacific (180° longitude). Clearly outline and label the four ENSO monitoring boxes: "
            "Niño 1+2 (10°S–0°, 90°W–80°W) along the South American coast, "
            "Niño 3 (5°N–5°S, 150°W–90°W), "
            "Niño 3.4 (5°N–5°S, 170°W–120°W) HIGHLIGHTED in bright red as the most important, "
            "and Niño 4 (5°N–5°S, 160°E–150°W). "
            "Show the equator as a horizontal line. Label the Western Pacific Warm Pool, Eastern Pacific Cold Tongue, and the South American continent. "
            "Clean cartographic style, white ocean, beige land, sharp boundary lines, legible serif labels. "
            "No clouds or weather, just the ocean basin and monitoring boxes. 1536×1024."
        ),
    ),
    (
        "walker_circulation.png",
        (
            "Educational cross-section diagram of the Walker circulation in the equatorial Pacific atmosphere, for a climate textbook. "
            "Side view (latitude=0) from Indonesia on the left to South America on the right, vertical axis showing the atmosphere from sea level to 15 km altitude. "
            "Show: (a) blue ocean below with a SLOPED thermocline that is deep on the left (Indonesian Warm Pool, ~28°C) and shallow on the right (eastern Pacific Cold Tongue, ~22°C); "
            "(b) curved arrows showing surface easterly trade winds blowing from east to west; "
            "(c) rising air column over Indonesia; (d) upper-level westerly return flow at altitude; "
            "(e) descending air column over the eastern Pacific. "
            "Label everything: Warm Pool, Cold Tongue, Thermocline, Trade Winds, Walker Cell, Rising/Descending branch. "
            "Sharp clean technical drawing, white background, light pastel colors, scientific style. 1536×1024."
        ),
    ),
    (
        "recharge_discharge_cycle.png",
        (
            "Educational four-panel cartoon showing the four phases of the ENSO recharge-discharge oscillator (Jin 1997), drawn for a climate science article. "
            "Each panel is a small side-view cartoon of the equatorial Pacific. "
            "Panel 1 (top-left): 'RECHARGED / Pre-El Niño' — thermocline is anomalously deep across the whole basin. "
            "Panel 2 (top-right): 'EL NIÑO' — thermocline flattens, warm pool sloshes east, SST anomalously warm in central/eastern Pacific (red color). "
            "Panel 3 (bottom-left): 'DISCHARGED / Post-El Niño' — thermocline shallow basin-wide, system depleted of heat content. "
            "Panel 4 (bottom-right): 'LA NIÑA' — thermocline steeply tilted, warm pool retreats west, east Pacific anomalously cool (blue color). "
            "Arrows showing meridional heat content transport between panels indicating the cycle direction. "
            "Clean schematic, light pastel colors, large labels. 1536×1024."
        ),
    ),
    (
        "enso_teleconnections.png",
        (
            "World map showing the global climate teleconnections during a typical El Niño winter (December-February), "
            "educational style for a climate science article. World map projection. "
            "Use color-coded regions and short text labels: "
            "'WET' (blue) over Peru/Ecuador coast, southern US/California, equatorial East Africa, southeast South America (Argentina/Uruguay/southern Brazil); "
            "'DRY' (orange) over Indonesia/northern Australia, southern Africa, northeast Brazil, Caribbean; "
            "'WARM' (red) over Alaska/western Canada, northeast US, southern Africa; "
            "'COOL' (light blue) over Florida/Gulf Coast. "
            "Label the Pacific Ocean with 'Warm SST anomaly' across the equatorial central/east Pacific. "
            "Clean cartographic style, light beige land, blue/orange shading for impacts. 1536×1024."
        ),
    ),
    (
        "spring_barrier_concept.png",
        (
            "Educational circular calendar infographic showing the seasonal cycle of the ENSO predictability barrier. "
            "Twelve-month wheel arranged as a clock, January at top going clockwise. "
            "Around the wheel, show two annotated curves: "
            "(1) the Bjerknes coupled-feedback strength as a thick blue curve, positive (growth) in August-November, near zero or negative (damping) in February-April; "
            "(2) westerly wind burst intensity as a thick orange curve, peaking in March-April. "
            "Highlight the SPRING (March-May) sector in pale red and label it 'PREDICTABILITY BARRIER — initial-condition information lost'. "
            "Highlight the AUTUMN (September-November) sector in pale green and label it 'GROWTH SEASON — strong air-sea coupling locks in event'. "
            "Add small arrow icons showing 'Forecast skill collapses crossing this window'. "
            "Clean infographic style, large clear labels, white background. 1536×1024."
        ),
    ),
]


def generate_and_save(filename: str, prompt: str) -> dict:
    out_path = OUTPUT_DIR / filename
    print(f"\n[{filename}] Requesting image from gpt-image-2 ...", flush=True)
    t0 = time.time()
    try:
        response = client.images.generate(
            model="gpt-image-2",
            prompt=prompt,
            size="1536x1024",
            n=1,
        )
        elapsed = time.time() - t0
        b64_data = response.data[0].b64_json
        if b64_data is None:
            raise ValueError("API returned no b64_json data")
        image_bytes = base64.b64decode(b64_data)
        out_path.write_bytes(image_bytes)
        size_kb = len(image_bytes) / 1024
        print(f"[{filename}] Saved {size_kb:.1f} KB in {elapsed:.1f}s", flush=True)
        return {"file": filename, "status": "ok", "size_kb": round(size_kb, 1), "elapsed_s": round(elapsed, 1)}
    except Exception as exc:
        elapsed = time.time() - t0
        print(f"[{filename}] FAILED after {elapsed:.1f}s: {exc}", flush=True)
        return {"file": filename, "status": "error", "error": str(exc), "elapsed_s": round(elapsed, 1)}


def main():
    if not os.environ.get("OPENAI_API_KEY"):
        print("ERROR: OPENAI_API_KEY is not set.", file=sys.stderr)
        sys.exit(1)

    results = []
    for filename, prompt in FIGURES:
        result = generate_and_save(filename, prompt)
        results.append(result)

    print("\n\n=== SUMMARY ===")
    for r in results:
        if r["status"] == "ok":
            print(f"  OK   {r['file']:40s}  {r['size_kb']:8.1f} KB  ({r['elapsed_s']}s)")
        else:
            print(f"  FAIL {r['file']:40s}  ERROR: {r['error']}")

    failures = [r for r in results if r["status"] != "ok"]
    if failures:
        print(f"\n{len(failures)} image(s) failed.")
        sys.exit(1)
    else:
        print(f"\nAll {len(results)} images generated successfully.")


if __name__ == "__main__":
    main()
