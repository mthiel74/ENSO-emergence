"""
Regenerate three ENSO educational illustrations using gpt-image-2.
Overwrites existing files in-place.
"""

import os
import base64
import time
from openai import OpenAI

client = OpenAI(api_key=os.environ["OPENAI_API_KEY"])

OUTPUT_DIR = "/Users/thiel/GitHub/ENSO-emergence/docs/images/figures-generated"

tasks = [
    {
        "filename": "walker_circulation.png",
        "prompt": (
            "Educational cross-section diagram of the Walker circulation in the equatorial Pacific atmosphere, "
            "for a climate textbook. Side view (latitude = 0) from Indonesia on the left to South America on the right. "
            "Vertical axis altitude 0 to 15 km. Show: (a) blue ocean below with a sloped thermocline that is DEEP on "
            "the left (Indonesian Warm Pool, label 'Deep Thermocline (~150 m)') and SHALLOWER on the right (Eastern "
            "Pacific Cold Tongue, label 'Shallow Thermocline (~50 m)'); (b) labeled 'Warm Pool ~28°C' on the west, "
            "'Cold Tongue ~22°C' on the east; (c) FOUR ARROWS forming a CLOSED COUNTER-CLOCKWISE CIRCULATION CELL: "
            "the SURFACE TRADE WINDS arrow points FROM RIGHT TO LEFT (east to west, i.e. blowing from South America "
            "toward Indonesia) labeled 'Trade Winds (Easterly)'; the LEFT branch is a thick UPWARD arrow over the "
            "Indonesian warm pool labeled 'Rising Branch' with a cumulus cloud; the UPPER-LEVEL RETURN FLOW arrow at "
            "altitude points FROM LEFT TO RIGHT (west to east, i.e. blowing from Indonesia back toward South America) "
            "labeled 'Upper-Level Westerly Return Flow' — this arrow MUST POINT IN THE OPPOSITE DIRECTION FROM THE "
            "SURFACE TRADE WIND ARROW BELOW IT; the RIGHT branch is a thick DOWNWARD arrow over the eastern Pacific "
            "labeled 'Descending Branch'. Together the four arrows trace a CLOSED RECTANGULAR LOOP, counter-clockwise "
            "when viewed with west on the left. Clean technical drawing, white background, light pastel colors. 1536x1024."
        ),
    },
    {
        "filename": "recharge_discharge_cycle.png",
        "prompt": (
            "Educational four-panel cartoon showing the four phases of the Jin 1997 ENSO recharge-discharge oscillator, "
            "drawn for a climate science article. The four panels are arranged in a 2x2 grid. Each panel is a side-view "
            "cartoon of the equatorial Pacific (Indonesia on left, South America on right) with sea surface and thermocline "
            "depth visible. Cycle ORDER: PANEL 1 (top-left) 'RECHARGED / Pre-El Niño' — thermocline is anomalously deep "
            "across the WHOLE basin (deep on both sides). PANEL 2 (top-right) 'EL NIÑO' — thermocline flattens, warm pool "
            "sloshes east, SST anomalously warm in central and eastern Pacific (color the eastern surface RED-ORANGE). "
            "PANEL 3 (bottom-right) 'DISCHARGED / Post-El Niño' — thermocline is shallow basin-wide, system depleted of "
            "heat content. PANEL 4 (bottom-left) 'LA NIÑA' — thermocline steeply tilted (deep on west, very shallow on "
            "east), warm pool retreats west, eastern Pacific anomalously cool (color east BLUE). CRITICAL: Connect the "
            "panels with FOUR ARROWS forming a CLEAR CLOCKWISE LOOP visible across the gaps between panels: arrow pointing "
            "RIGHT from Panel 1 to Panel 2; arrow pointing DOWN from Panel 2 to Panel 3; arrow pointing LEFT from Panel 3 "
            "to Panel 4; arrow pointing UP from Panel 4 back to Panel 1. Place a small 'ENSO RECHARGE-DISCHARGE OSCILLATOR "
            "(Jin 1997)' caption in the center of the 2x2 grid. The cycle arrows must be unambiguous so the reader can "
            "trace 1→2→3→4→1 clockwise. Clean schematic, light pastel colors, large labels. 1536x1024."
        ),
    },
    {
        "filename": "enso_teleconnections.png",
        "prompt": (
            "World map showing the global climate teleconnections during a typical El Niño winter (December-February), "
            "educational style for a climate science article. World map projection. Use color-coded regions with short "
            "text labels. WET (blue label) over: Peruvian/Ecuadorian coast; southern US / California; EQUATORIAL EAST "
            "AFRICA (Kenya, Tanzania, Ethiopia — this region MUST be labeled WET, not dry, because the displaced ITCZ "
            "and warm western Indian Ocean enhance East African short rains during El Niño); southeast South America "
            "(Argentina, Uruguay, southern Brazil). DRY (orange label) over: Indonesia and northern Australia; southern "
            "Africa (Zambia, Zimbabwe, Mozambique, South Africa); northeast Brazil; the Caribbean. WARM (red label) over: "
            "Alaska and western Canada; northeast United States; also southern Africa gets a WARM label adjacent to its "
            "DRY label (warm and dry, as is typical for El Niño austral summer over southern Africa). COOL (light blue "
            "label) over: Florida and US Gulf Coast. Across the equatorial central and eastern Pacific Ocean, draw a red "
            "shaded band labeled 'Warm SST anomaly'. Clean cartographic style, light beige land, blue / orange / red "
            "shading for impacts. CRITICAL: equatorial East Africa must be WET (blue), not DRY (orange). 1536x1024."
        ),
    },
]

for i, task in enumerate(tasks, 1):
    out_path = os.path.join(OUTPUT_DIR, task["filename"])
    print(f"\n[{i}/3] Generating {task['filename']} ...")
    t0 = time.time()

    response = client.images.generate(
        model="gpt-image-2",
        prompt=task["prompt"],
        size="1536x1024",
        n=1,
    )

    image_data = response.data[0].b64_json
    image_bytes = base64.b64decode(image_data)

    with open(out_path, "wb") as f:
        f.write(image_bytes)

    elapsed = time.time() - t0
    size_kb = os.path.getsize(out_path) / 1024
    print(f"  Saved {out_path}")
    print(f"  Size: {size_kb:.1f} KB  |  Time: {elapsed:.1f}s")

print("\nAll three images regenerated successfully.")
