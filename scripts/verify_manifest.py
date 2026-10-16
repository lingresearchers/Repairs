#!/usr/bin/env python3
"""Check released SHA-256 hashes; requires only Python's standard library."""
import argparse
import hashlib
from pathlib import Path, PurePosixPath
import zipfile


def digest_stream(stream):
    h = hashlib.sha256()
    while True:
        data = stream.read(1024 * 1024)
        if not data:
            return h.hexdigest()
        h.update(data)


def entries(text):
    for line in text.splitlines():
        if not line.strip():
            continue
        expected, relative = line.split("  ", 1)
        path = PurePosixPath(relative)
        if path.is_absolute() or ".." in path.parts or len(expected) != 64:
            raise ValueError("Invalid manifest entry")
        yield expected, relative


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--zip", type=Path, help="Verify the distributed ZIP directly")
    args = parser.parse_args()
    failures = []
    count = 0
    if args.zip:
        with zipfile.ZipFile(args.zip) as z:
            manifest = "Repair_Supplementary_Materials/MANIFEST.sha256"
            for expected, relative in entries(z.read(manifest).decode("utf-8")):
                name = "Repair_Supplementary_Materials/" + relative
                with z.open(name) as stream:
                    actual = digest_stream(stream)
                count += 1
                if actual != expected:
                    failures.append(relative)
    else:
        root = args.root.resolve()
        text = (root / "MANIFEST.sha256").read_text(encoding="utf-8")
        for expected, relative in entries(text):
            path = root.joinpath(*PurePosixPath(relative).parts)
            count += 1
            if not path.is_file():
                failures.append(relative + " (missing)")
                continue
            with path.open("rb") as stream:
                actual = digest_stream(stream)
            if actual != expected:
                failures.append(relative)
    if failures:
        for name in failures:
            print("FAILED:", name)
        raise SystemExit(1)
    print(f"All {count} released file hashes match.")


if __name__ == "__main__":
    main()
