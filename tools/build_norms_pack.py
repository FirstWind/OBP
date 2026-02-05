import json
import re
import subprocess
from pathlib import Path
from datetime import datetime, timezone

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "out"
DOCS_NORMS = ROOT / "docs" / "norms" / "rfp2015_v1"

PDF = ROOT / "315 полный.pdf"

POPPLER = Path(r"C:\Program Files\poppler-24.08.0\Library\bin\pdftotext.exe")


def run_pdftotext(args):
    if POPPLER.exists():
        cmd = [str(POPPLER)] + args
    else:
        cmd = ["pdftotext"] + args
    subprocess.check_call(cmd, cwd=str(ROOT))


def extract_texts():
    OUT.mkdir(exist_ok=True)
    run_pdftotext([str(PDF), str(OUT / "315_full.txt")])
    run_pdftotext(["-layout", str(PDF), str(OUT / "315_full_layout.txt")])
    run_pdftotext(["-raw", str(PDF), str(OUT / "315_full_raw.txt")])

    full = (OUT / "315_full.txt").read_text(encoding="utf-8", errors="ignore")
    layout = (OUT / "315_full_layout.txt").read_text(encoding="utf-8", errors="ignore")
    raw = (OUT / "315_full_raw.txt").read_text(encoding="utf-8", errors="ignore")

    def slice_block(text, start, end, out_path):
        s = text.find(start)
        if s == -1:
            raise RuntimeError(f"start marker not found: {start}")
        e = text.find(end, s)
        if e == -1:
            e = len(text)
        out_path.write_text(text[s:e], encoding="utf-8")

    slice_block(layout, "Приложение № 10", "Приложение № 11", OUT / "appendix10_layout.txt")
    slice_block(layout, "Приложение № 11", "Приложение № 12", OUT / "appendix11_layout.txt")
    slice_block(raw, "Приложение № 12", "Приложение № 13", OUT / "appendix12_raw.txt")
    slice_block(layout, "Приложение № 13", "Приложение № 14", OUT / "appendix13_layout.txt")


def normalize_ex_id(id_str: str) -> int:
    if "." in id_str:
        parts = id_str.split(".")
        return int(parts[0]) * 10 + int(parts[1])
    return int(id_str)


def parse_appendix10():
    text = (OUT / "appendix10_layout.txt").read_text(encoding="utf-8", errors="ignore")
    lines = [ln.rstrip() for ln in text.splitlines()]

    header_patterns = [
        r"^Приложение",
        r"^к Руководству",
        r"^Перечень",
        r"^Наименование упражнения",
        r"^Возрастная группа",
        r"^упражнения$",
        r"^мужчины$",
        r"^женщины$",
        r"^№$",
        r"^\d+\s+\d+\s+\d+\s+\d+\s+\d+.*",
        r"^дополнительно",
        r"^Основные физические качества",
        r"^Специальные физические навыки",
    ]

    def is_header_line(l):
        if not l:
            return True
        for p in header_patterns:
            if re.match(p, l, re.IGNORECASE):
                return True
        if "мужчины" in l.lower() and "женщины" in l.lower():
            return True
        if re.match(r"^(до|лет|года|старше|25|29|30|34|35|39|40|44|45|49|50|54|55)\b", l):
            return True
        if re.match(r"^[\d\s–-]+$", l):
            return True
        return False

    sections_map = {
        "ловкость": "lovkost",
        "быстрота": "bystrota",
        "сила": "sila",
        "рукопашный бой": "rukopashnyi_boi",
        "выносливость": "vynoslivost",
        "преодоление препятствий": "preodolenie_prepyatstvii",
        "служебно-прикладное плавание": "plavanie",
        "упражнения в составе подразделения": "podrazdelenie",
        "специальные физические навыки": "special",
    }

    rows = []
    section = "unknown"

    time_ids = {10, 20, 23, 24, 25, 26, 30, 31, 32, 33, 34, 35, 36, 37, 39, 40, 41, 42, 43, 44, 49, 491, 50, 51, 52, 53, 54, 55}
    distance_ids = {47, 56, 45}
    reps_ids = {4, 5, 6, 7, 8, 9, 11, 12, 13, 16, 17, 18, 19, 21, 22}

    def result_meta(ex_id):
        if ex_id in time_ids:
            return ("time", "sec")
        if ex_id in distance_ids:
            unit = "cm" if ex_id == 45 else "m"
            return ("distance", unit)
        if ex_id in reps_ids:
            return ("reps", "reps")
        return ("score", "points")

    for ln in lines:
        l = ln.strip()
        if not l:
            continue
        low = l.lower()
        if low in sections_map:
            section = sections_map[low]
            continue
        if low.startswith("упражнения в составе подразделения"):
            section = "podrazdelenie"
            continue
        if low.startswith("служебно-прикладное плавание"):
            section = "plavanie"
            continue
        if low.startswith("преодоление препятствий"):
            section = "preodolenie_prepyatstvii"
            continue
        if low.startswith("рукопашный бой"):
            section = "rukopashnyi_boi"
            continue
        if low.startswith("выносливость"):
            section = "vynoslivost"
            continue
        if low.startswith("быстрота"):
            section = "bystrota"
            continue
        if low.startswith("сила"):
            section = "sila"
            continue
        if low.startswith("ловкость"):
            section = "lovkost"
            continue
        if low.startswith("специальные физические навыки"):
            section = "special"
            continue
        if is_header_line(l):
            continue

        if re.search(r"\b\d+(?:\.\d+)?\b", l) and re.search(r"[+-]", l):
            m = re.search(r"(\d+(?:\.\d+)?)\s+([+-].*)$", l)
            if not m:
                continue
            id_str = m.group(1)
            tail = m.group(2)
            name = l[: m.start(1)].strip()
            name = re.sub(r"\s+", " ", name)
            signs = re.findall(r"[+-]", tail)
            ex_id = normalize_ex_id(id_str)
            rows.append(
                {
                    "exercise_id": ex_id,
                    "name": name,
                    "section": section,
                    "signs": signs,
                }
            )
        else:
            if rows:
                rows[-1]["name"] = (rows[-1]["name"] + " " + l).strip()

    # convert to final exercises with allowed map
    exercises = []
    for r in rows:
        if len(r["signs"]) != 14:
            raise RuntimeError(f"Unexpected signs count for {r['exercise_id']}: {len(r['signs'])}")
        allowed_m = {str(i + 1): (r["signs"][i] == "+") for i in range(8)}
        allowed_f = {str(i + 1): (r["signs"][8 + i] == "+") for i in range(6)}
        ex_id = r["exercise_id"]
        result_type, unit = result_meta(ex_id)
        attempt_policy = "two_if_fall" if ex_id in {10, 23, 24, 25, 26} else "single"
        is_group = r["section"] == "podrazdelenie" or "в составе подразделения" in r["name"].lower()
        exercises.append(
            {
                "exercise_id": ex_id,
                "name": r["name"],
                "section": r["section"],
                "result_type": result_type,
                "unit": unit,
                "is_group": is_group,
                "attempt_policy": attempt_policy,
                "allowed": {"M": allowed_m, "F": allowed_f},
            }
        )

    return exercises


def parse_appendix11():
    text = (OUT / "appendix11_layout.txt").read_text(encoding="utf-8", errors="ignore")
    lines = [ln.rstrip() for ln in text.splitlines()]

    age_re = re.compile(r"(\d)\s+возрастной группы")
    row_re = re.compile(r"\s*(\d)\s+([0-9]+)\s+(.+)")
    row_with_age_re = re.compile(r"(\d)\s+возрастной группы\s+(\d)\s+([0-9]+)\s+(.+)")

    def parse_row_tokens(rest):
        toks = [t for t in rest.strip().split() if t]
        out = []
        for t in toks:
            if t == "-" or re.match(r"^\d+$", t):
                out.append(t)
        return out

    next_age = [None] * len(lines)
    for i in range(len(lines) - 1, -1, -1):
        l = lines[i].strip()
        if l:
            m = age_re.search(l)
            if m:
                next_age[i] = int(m.group(1))
            else:
                next_age[i] = next_age[i + 1] if i + 1 < len(lines) else None
        else:
            next_age[i] = next_age[i + 1] if i + 1 < len(lines) else None

    current_sex = "M"
    current_age = None
    rows = []
    for i, ln in enumerate(lines):
        l = ln.strip()
        if not l:
            continue
        low = l.lower()
        if "женского пола" in low:
            current_sex = "F"
        if low.startswith("военнослужащие") and "женского" not in low:
            current_sex = "M"
        m_age = age_re.search(l)
        if m_age:
            current_age = int(m_age.group(1))

        m2 = row_with_age_re.search(l)
        if m2:
            age = int(m2.group(1))
            cat = int(m2.group(2))
            min_points = int(m2.group(3))
            tokens = parse_row_tokens(m2.group(4))
            if len(tokens) >= 12:
                rows.append(
                    {
                        "sex": current_sex,
                        "age_group": age,
                        "category": cat,
                        "min_points": min_points,
                        "tokens": tokens[:12],
                    }
                )
            continue

        m = row_re.match(l)
        if m:
            cat = int(m.group(1))
            min_points = int(m.group(2))
            tokens = parse_row_tokens(m.group(3))
            if len(tokens) >= 12:
                age = current_age
                if cat == 3 and next_age[i] is not None and next_age[i] != current_age:
                    age = next_age[i]
                rows.append(
                    {
                        "sex": current_sex,
                        "age_group": age,
                        "category": cat,
                        "min_points": min_points,
                        "tokens": tokens[:12],
                    }
                )

    thresholds = []
    for r in rows:
        t = r["tokens"]
        groups = [t[0:3], t[3:6], t[6:9]]
        qual = t[9:12]
        n_required = None
        grades = None
        if all(x != "-" for x in groups[0]):
            n_required = 3
            grades = groups[0]
        elif all(x != "-" for x in groups[1]):
            n_required = 4
            grades = groups[1]
        elif all(x != "-" for x in groups[2]):
            n_required = 5
            grades = groups[2]
        else:
            raise RuntimeError(f"Cannot determine N_required for row {r}")

        thresholds.append(
            {
                "sex": r["sex"],
                "age_group": r["age_group"],
                "category": r["category"],
                "n_required": n_required,
                "min_points_per_exercise": r["min_points"],
                "grades": {
                    "excellent": int(grades[0]),
                    "good": int(grades[1]),
                    "satisfactory": int(grades[2]),
                },
                "qualification": {
                    "level_1": int(qual[0]),
                    "level_2": int(qual[1]),
                    "level_3": int(qual[2]),
                },
            }
        )

    return thresholds


def parse_appendix13():
    text = (OUT / "appendix13_layout.txt").read_text(encoding="utf-8", errors="ignore")
    lines = [ln.rstrip() for ln in text.splitlines() if ln.strip()]

    current_sex = "M"
    current_age = None
    rows = []

    next_age = [None] * len(lines)
    for i in range(len(lines) - 1, -1, -1):
        l = lines[i].strip()
        if l:
            m = re.search(r"(\d) возрастной группы", l)
            if m:
                next_age[i] = int(m.group(1))
            else:
                next_age[i] = next_age[i + 1] if i + 1 < len(lines) else None
        else:
            next_age[i] = next_age[i + 1] if i + 1 < len(lines) else None

    row_pat = re.compile(r"\b([1-3])\s+(\d+)\s+(\d+)\s+(\d+)\b")

    for i, l in enumerate(lines):
        low = l.lower()
        if "женского пола" in low:
            current_sex = "F"
        m_age = re.search(r"(\d) возрастной группы", l)
        if m_age:
            current_age = int(m_age.group(1))
        # row line: category + excellent + good + satisfactory
        m = row_pat.search(l)
        if m:
            cat = m.group(1)
            age = current_age
            if cat == "3" and next_age[i] is not None and next_age[i] != current_age:
                age = next_age[i]
            rows.append(
                {
                    "sex": current_sex,
                    "age_group": age,
                    "category": int(cat),
                    "grades": {
                        "excellent": int(m.group(2)),
                        "good": int(m.group(3)),
                        "satisfactory": int(m.group(4)),
                    },
                }
            )

    return rows


def parse_appendix12():
    text = (OUT / "appendix12_raw.txt").read_text(encoding="utf-8", errors="ignore")
    parts = re.split(r"\n\s*Таблица\s*(\d+)\s*\n", text)

    def get_ex_ids(content):
        lines = [ln.strip() for ln in content.splitlines()]
        ex_ids = []
        state = 0
        for ln in lines:
            if re.match(r"^100\s", ln):
                break
            if ln.lower().startswith("упражнение"):
                state = 1
                continue
            if state == 1 and ln == "№":
                state = 2
                continue
            if state == 2:
                m = re.match(r"^(\d+(?:\.\d+)?)\.?$", ln)
                if m:
                    ex_ids.append(m.group(1))
                state = 0
                continue
        return ex_ids

    def parse_rows(content):
        rows = []
        for ln in content.splitlines():
            ln = ln.strip()
            if re.match(r"^\d+\s", ln):
                parts = ln.split()
                try:
                    points = int(parts[0])
                except ValueError:
                    continue
                values = parts[1:]
                rows.append((points, values))
        return rows

    def to_number(s):
        if s == "-":
            return None
        s = s.replace(",", ".")
        try:
            return float(s)
        except ValueError:
            return None

    def to_seconds(val):
        if val is None:
            return None
        s = f"{val}"
        if "." in s:
            a, b = s.split(".", 1)
            if len(b) == 2 and 0 <= int(b) <= 59 and 1 <= int(a) <= 59:
                return int(a) * 60 + int(b)
        return float(val)

    # exercise metadata (result_type, unit, better_is)
    time_ids = {10, 20, 23, 24, 25, 26, 30, 31, 32, 33, 34, 35, 36, 37, 39, 40, 41, 42, 43, 44, 49, 491, 50, 51, 52, 53, 54, 55}
    distance_ids = {47, 56, 45}
    reps_ids = {4, 5, 6, 7, 8, 9, 11, 12, 13, 16, 17, 18, 19, 21, 22}

    def result_meta(ex_id):
        if ex_id in time_ids:
            return ("time", "sec", "lower")
        if ex_id in distance_ids:
            unit = "cm" if ex_id == 45 else "m"
            return ("distance", unit, "higher")
        if ex_id in reps_ids:
            return ("reps", "reps", "higher")
        # fallback
        return ("score", "points", "higher")

    scales = []
    for i in range(1, len(parts), 2):
        tno = parts[i]
        content = parts[i + 1]
        sex = "M" if int(tno) <= 4 else "F"
        ex_ids_raw = get_ex_ids(content)
        ex_ids = [normalize_ex_id(x) for x in ex_ids_raw]

        # build column definitions
        columns = []
        if tno == "1":
            base = [4, 5, 6, 7, 8, 9, 11, 12, 13]
            columns.extend([(x, "base") for x in base])
            columns.extend([(16, "wt_le70"), (16, "wt_gt70")])
            columns.extend([(17, "wt_le70"), (17, "wt_gt70")])
            columns.extend([(18, "wt_le70"), (18, "wt_gt70")])
            columns.extend([(19, "wt_le70"), (19, "wt_70_100"), (19, "wt_gt100")])
            columns.append((20, "base"))
        elif tno == "2":
            columns = [(x, "base") for x in ex_ids]
        elif tno == "3":
            columns = [
                (30, "base"),
                (32, "base"),
                (33, "base"),
                (34, "distance_1100"),
                (34, "distance_3100"),
                (42, "base"),
                (43, "base"),
                (51, "unit_le_10"),
                (51, "unit_gt_10"),
                (52, "unit_le_10"),
                (52, "unit_gt_10"),
            ]
        elif tno == "4":
            columns = [(x, "base") for x in ex_ids]
        elif tno == "5":
            columns = [(x, "base") for x in ex_ids]
        elif tno == "6":
            columns = [(x, "base") for x in ex_ids]
        else:
            columns = [(x, "base") for x in ex_ids]

        # parse rows
        rows = parse_rows(content)
        # filter to points 0..100
        rows = [(p, v) for p, v in rows if 0 <= p <= 100]

        # build per-column scale rows
        col_rows = {idx: [] for idx in range(len(columns))}
        for points, vals in rows:
            if len(vals) < len(columns):
                continue
            for idx, raw_val in enumerate(vals[: len(columns)]):
                val = to_number(raw_val)
                if val is None:
                    continue
                ex_id, variant = columns[idx]
                rtype, unit, better_is = result_meta(ex_id)
                if rtype == "time":
                    val = to_seconds(val)
                col_rows[idx].append((val, points))

        # emit scales (replicate for all age groups)
        for idx, col in enumerate(columns):
            ex_id, variant = col
            rtype, unit, better_is = result_meta(ex_id)
            rows_list = col_rows[idx]
            if not rows_list:
                continue
            # sort for deterministic output by points descending
            rows_list.sort(key=lambda x: -x[1])
            for age_group in (range(1, 9) if sex == "M" else range(1, 7)):
                scale_id = f"ex{ex_id}_{sex}_{age_group}_{variant}"
                scale_rows = []
                for val, pts in rows_list:
                    if better_is == "lower":
                        scale_rows.append({"result_lte": round(val, 3), "points": int(pts)})
                    else:
                        scale_rows.append({"result_gte": round(val, 3), "points": int(pts)})
                scales.append(
                    {
                        "scale_id": scale_id,
                        "exercise_id": ex_id,
                        "sex": sex,
                        "age_group": age_group,
                        "variant": variant,
                        "result_type": rtype,
                        "unit": unit,
                        "better_is": better_is,
                        "rows": scale_rows,
                    }
                )

    return scales


def build_manifest():
    # hash pdf
    import hashlib

    data = PDF.read_bytes()
    sha = hashlib.sha256(data).hexdigest()
    return {
        "norms_id": "rfp2015_v1",
        "title": "РФП-2015 (315 полный)",
        "source": {
            "document_name": "315 полный.pdf",
            "document_fingerprint": f"sha256:{sha}",
            "extraction_notes": "Built from PDF via pdftotext (raw/layout) and heuristic parsing",
        },
        "created_at": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "version": "1.0.0",
        "compatible_app_versions": [">=1.0.0"],
        "files": {
            "appendix10": "appendix10.json",
            "appendix11": "appendix11.json",
            "appendix12": "appendix12.json",
            "appendix13": "appendix13.json",
        },
    }


def main():
    extract_texts()

    exercises = parse_appendix10()
    thresholds = parse_appendix11()
    exercise_grades = parse_appendix13()
    scales = parse_appendix12()

    DOCS_NORMS.mkdir(parents=True, exist_ok=True)

    (DOCS_NORMS / "manifest.json").write_text(json.dumps(build_manifest(), ensure_ascii=False, indent=2), encoding="utf-8")
    (DOCS_NORMS / "appendix10.json").write_text(json.dumps({"exercises": exercises, "metadata": {"age_groups": {"M": [1,2,3,4,5,6,7,8], "F": [1,2,3,4,5,6]}}}, ensure_ascii=False, indent=2), encoding="utf-8")
    (DOCS_NORMS / "appendix11.json").write_text(json.dumps({"thresholds": thresholds}, ensure_ascii=False, indent=2), encoding="utf-8")
    (DOCS_NORMS / "appendix12.json").write_text(json.dumps({"scales": scales, "adjustments": {"rules": [], "max_adjustments_per_result": 2}}, ensure_ascii=False, indent=2), encoding="utf-8")
    (DOCS_NORMS / "appendix13.json").write_text(json.dumps({"exercise_grades": exercise_grades}, ensure_ascii=False, indent=2), encoding="utf-8")

    print("DONE:", DOCS_NORMS)


if __name__ == "__main__":
    main()
