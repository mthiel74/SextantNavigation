"""
Generate ILLUSTRATIVE (non-scientific) PNGs for the SextantNavigation post
using OpenAI gpt-image-2. These are atmospheric/conceptual ART to help the
reader connect to the science — NOT data plots or labelled diagrams (those are
all made in Mathematica). Each prompt explicitly forbids text/labels/diagrams.

Run: python generate_illustrations.py
Requires: OPENAI_API_KEY in environment, openai Python SDK installed.
Outputs are committed; rebuilding the notebook does NOT call OpenAI.
"""

import base64
import os
import sys
import time
from pathlib import Path

from openai import OpenAI

OUTPUT_DIR = Path(os.environ.get("OUT_DIR", Path(__file__).resolve().parent))
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

client = OpenAI()

NO_TEXT = (
    " Purely illustrative fine-art image with NO text, NO labels, NO numbers, "
    "NO diagrams, NO arrows, NO charts — atmosphere and scene only."
)

FIGURES = [
    (
        "illus_navigator_sextant.png",
        "A lone ship's navigator standing on the weathered wooden deck of a 19th-century "
        "sailing vessel at first light, raising a polished brass sextant to one eye to measure "
        "the altitude of the rising Sun above a calm ocean horizon. Warm golden dawn light, "
        "soft sea haze, rigging and a furled sail behind, painterly cinematic maritime oil-painting "
        "style, romantic and serene." + NO_TEXT,
    ),
    (
        "illus_harrison_chronometer.png",
        "A beautiful antique marine chronometer in the style of John Harrison's H4 — a large "
        "polished brass pocket-watch-like timekeeper with a white enamel dial and ornate blued "
        "hands — resting in its brass-gimballed wooden box on a navigator's chart table beside brass "
        "dividers, a rolled nautical chart and a candle. Warm museum lighting, rich mahogany and "
        "brass, exquisite detail, classical still-life painting style." + NO_TEXT,
    ),
    (
        "illus_lunar_distance.png",
        "A ship's officer at deep twilight on a tall-ship deck, sighting through a brass sextant to "
        "measure the angle between a luminous crescent Moon and a single bright star in a darkening "
        "indigo sky scattered with faint early stars, the sea black and glassy below. Moody, "
        "atmospheric, painterly, a sense of quiet concentration and wonder." + NO_TEXT,
    ),
    (
        "illus_james_caird.png",
        "A small open wooden lifeboat with a tattered improvised sail, crowded with exhausted "
        "men in oilskins, riding enormous grey Southern Ocean swells under a wild storm-torn sky, "
        "one figure bracing himself to take a sextant sight in a brief break of sunlight — evoking "
        "Shackleton's 1916 James Caird voyage. Dramatic, heroic, cold spray, dramatic maritime "
        "history painting." + NO_TEXT,
    ),
    (
        "illus_celestial_dome.png",
        "A dreamlike wide view of a small sailing ship alone on a calm midnight sea beneath an "
        "immense dome of stars, the Milky Way arching overhead, faint constellations and a low "
        "horizon all around, the vast celestial sphere enclosing the tiny vessel. Ethereal, awe-"
        "inspiring, deep blues and silver starlight, romantic astronomical painting." + NO_TEXT,
    ),
]


def generate_and_save(filename: str, prompt: str) -> dict:
    out_path = OUTPUT_DIR / filename
    print(f"\n[{filename}] Requesting image from gpt-image-2 ...", flush=True)
    t0 = time.time()
    try:
        response = client.images.generate(
            model="gpt-image-2", prompt=prompt, size="1536x1024", n=1
        )
        elapsed = time.time() - t0
        b64_data = response.data[0].b64_json
        if b64_data is None:
            raise ValueError("API returned no b64_json data")
        image_bytes = base64.b64decode(b64_data)
        out_path.write_bytes(image_bytes)
        size_kb = len(image_bytes) / 1024
        print(f"[{filename}] Saved {size_kb:.1f} KB in {elapsed:.1f}s", flush=True)
        return {"file": filename, "status": "ok"}
    except Exception as exc:
        print(f"[{filename}] FAILED: {exc}", flush=True)
        return {"file": filename, "status": "error", "error": str(exc)}


def main():
    if not os.environ.get("OPENAI_API_KEY"):
        print("ERROR: OPENAI_API_KEY is not set.", file=sys.stderr)
        sys.exit(1)
    results = [generate_and_save(f, p) for f, p in FIGURES]
    print("\n=== SUMMARY ===")
    for r in results:
        print(f"  {r['status'].upper():5s} {r['file']}" + (f"  {r.get('error','')}" if r['status'] != 'ok' else ""))
    if any(r["status"] != "ok" for r in results):
        sys.exit(1)


if __name__ == "__main__":
    main()
