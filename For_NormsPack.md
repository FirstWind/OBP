Лучше **сразу иметь стартовый `Norms Pack`**, извлечённый из **315 полный.pdf**, а не пытаться «зашивать» правила в код и не откладывать JSON “на потом”.

Почему так лучше (для Cursor/Codex и вообще):

### Вариант A — стартовый Norms Pack из PDF (рекомендую)

**Плюсы**

* **Детерминированность**: расчёты всегда опираются на конкретные таблицы/пороги из пакета.
* **Аудит и воспроизводимость**: в сессии фиксируем `norms_id` + `hash`, можно доказать «по каким нормам считали».
* **Обновляемость**: если нормы меняются/уточняются — меняется только пакет, а не код.
* **Быстрее тестировать**: можно сразу сделать эталонные кейсы “ввод → баллы → итог”.

**Минусы**

* Нужно один раз аккуратно извлечь данные из PDF и привести к структуре.

➡️ Итог: **делаем “v1 Norms Pack” из PDF + Schema + 10–20 golden test cases**.

---

### Вариант B — “дадите готовые JSON” без привязки к PDF

Это возможно, но тогда возникают два риска:

1. **несостыковка с фактическим документом**, если в JSON случайно упущены примечания/исключения/границы;
2. **сложно защищать результат**, если кто-то спросит “откуда пороги/баллы”.

➡️ Этот вариант норм только если у вас уже есть утверждённый набор нормативов в машиночитаемом виде (или вы готовы принять “примерные” данные — что для РФП обычно нельзя).

---

## Как лучше в проекте (практика)

1. **Сначала**: извлекаем из 315 полный.pdf “минимально достаточный” Norms Pack:

   * список упражнений (id, название, единицы измерения, применимость);
   * таблицы баллов/порогов (appendix12/11/13 по вашей структуре).
2. **Параллельно**: фиксируем JSON Schema + версионирование (`norms_id`, `source_doc`, `pages`, `hash`).
3. **Сразу после**: делаем golden tests (набор “ожидаемых” результатов).

---

## Что отвечать Cursor’у коротко (готовая формулировка)

Можно прямо так:

> Нужен стартовый Norms Pack (JSON), извлечённый из 315 полный.pdf. Это обеспечит детерминированность, аудит и возможность обновлять нормы без изменения кода. Код не должен содержать нормативные таблицы — только загрузку/валидацию пакета и расчёт по данным. В репозитории должен быть `docs/norms/rfp2015_v1/` с `manifest.json` и appendix*.json + JSON Schema + golden test cases.

---

Если хотите, я могу:

* оформить **единый “контракт” Norms Pack** (manifest + schema + правила именования),
* и составить **план извлечения из PDF** так, чтобы Cursor/Codex мог автоматизировать перенос таблиц в JSON без ручной боли.



Делаем так: **сначала — стартовый Norms Pack из `315 полный.pdf`**, а Cursor/Codex пусть реализует **две вещи**:

1. загрузку/валидацию пакета, 2) расчёт строго по данным.
   И отдельно — **утилита извлечения** (одноразовая, но лучше оставить в `/tools/`).

Ниже даю **(А) контракт Norms Pack (JSON-структуры)** и **(Б) практичный план извлечения из PDF**, так чтобы это можно было прямо скормить Cursor’у как задачу.

---

# A) Контракт стартового Norms Pack (v1)

Структура папки:

```
docs/norms/rfp2015_v1/
  manifest.json
  appendix10.json
  appendix11.json
  appendix12.json
  appendix13.json
  schemas/
    manifest.schema.json
    appendix10.schema.json
    appendix11.schema.json
    appendix12.schema.json
    appendix13.schema.json
```

## A1. manifest.json

**Задача:** зафиксировать происхождение пакета и сделать его воспроизводимым.

Минимальная схема:

```json
{
  "norms_id": "rfp2015_v1",
  "title": "РФП-2015 (315 полный) — нормативы ФП",
  "source": {
    "document_name": "315 полный.pdf",
    "document_fingerprint": "sha256:<hash_of_pdf>",
    "extraction_notes": "таблицы приложений 10-13 перенесены в JSON; проверено выборочно"
  },
  "created_at": "2026-02-01T00:00:00Z",
  "version": "1.0.0",
  "compatible_app_versions": [">=1.0.0"],
  "files": {
    "appendix10": "appendix10.json",
    "appendix11": "appendix11.json",
    "appendix12": "appendix12.json",
    "appendix13": "appendix13.json"
  }
}
```

---

## A2. appendix10.json — каталог упражнений + применимость

В PDF есть матрица применимости упражнений по возрастным группам и полу (в виде «+/-» в таблице приложения). 

Рекомендованная структура:

```json
{
  "exercises": [
    {
      "exercise_id": 39,
      "name": "Бег на 1 км",
      "section": "выносливость",
      "result_type": "time",
      "unit": "sec",
      "is_group": false,
      "attempt_policy": "single",
      "allowed": {
        "M": { "1": true, "2": true, "3": true, "4": true, "5": true, "6": true, "7": true, "8": true },
        "F": { "1": true, "2": true, "3": true, "4": true, "5": true, "6": true }
      }
    }
  ],
  "metadata": {
    "age_groups": {
      "M": [1,2,3,4,5,6,7,8],
      "F": [1,2,3,4,5,6]
    }
  }
}
```

### Важные поля

* `section`: одна из фиксированных групп (ловкость, быстрота, сила, рукопашный бой, выносливость, препятствия, спец.навыки/плавание/лыжи/марш и т.п.). Порядок выполнения в системе вам уже нужен как «стандартная последовательность» (это в ТЗ отдельно).
* `attempt_policy`:

  * `"single"` для большинства;
  * `"two_if_fall"` для №10/23/24/25/26 (как в вашем ТЗ).
* `result_type`: `"time" | "reps" | "distance" | "score" | "mixed"`
* `allowed`: применимость по матрице приложений. 

---

## A3. appendix11.json — итоговые пороги (сумма баллов) + минимальный порог за упражнение

Из PDF: итоговая оценка определяется по таблице (приложение №11) и **при условии выполнения минимального порога в каждом упражнении**. 

Рекомендуемая структура (универсальная под 3/4/5 упражнений):

```json
{
  "thresholds": [
    {
      "sex": "M",
      "age_group": 3,
      "category": 1,
      "n_required": 5,
      "min_points_per_exercise": 10,
      "grades": {
        "excellent": 250,
        "good": 200,
        "satisfactory": 150
      }
    }
  ]
}
```

> Примечание: конкретные числа берутся из таблицы приложения №11 — в код **не зашивать**.

---

## A4. appendix12.json — шкалы “результат → баллы” по каждому упражнению

Из PDF: начисление баллов — по таблицам приложения №12. 
И важно: в документе упоминаются **поправки к результатам** (по форме одежды/условиям среды и т.п.). Это лучше тоже положить в Norms Pack как отдельный блок, иначе потом “вылезет” как неучтённое требование. 

Структура для шкал (поддерживает диапазоны и дискретные значения):

```json
{
  "scales": [
    {
      "scale_id": "ex39_M_3_base",
      "exercise_id": 39,
      "sex": "M",
      "age_group": 3,
      "variant": "base",
      "result_type": "time",
      "unit": "sec",
      "better_is": "lower",
      "rows": [
        { "result_lte": 210, "points": 100 },
        { "result_lte": 220, "points": 95 }
      ]
    }
  ],
  "adjustments": {
    "rules": [
      {
        "adjustment_id": "clothes_a",
        "applies_to": ["time", "distance"],
        "description": "Поправка а) форма одежды ...",
        "effect": { "type": "delta_seconds", "value": 5 }
      }
    ],
    "max_adjustments_per_result": 2
  }
}
```

**Почему adjustments лучше включить сразу:** в PDF явно сказано, что может быть до двух поправок, одна по форме одежды, одна по условиям среды, и они суммируются. 
Если в v1 вы пока не реализуете поправки в UI — всё равно храните их в pack, а в приложении включите флаг `AdjustmentsPolicy = disabled` (и покажите “функция не активна”).

---

## A5. appendix13.json — перевод баллов за одно упражнение в оценку

В PDF видно, что **приложение №13** — это «таблица перевода баллов, набранных в одном упражнении, в оценку». 

Структура:

```json
{
  "points_to_grade": [
    { "min_points": 0, "grade": "unsatisfactory" },
    { "min_points": 30, "grade": "satisfactory" },
    { "min_points": 60, "grade": "good" },
    { "min_points": 90, "grade": "excellent" }
  ]
}
```

---

# B) План извлечения Norms Pack из `315 полный.pdf` (чтобы Cursor сделал без боли)

## B0. Цель извлечения

Сделать **v1 Norms Pack**, который:

* покрывает приложения №10–13 (каталог упражнений, итоговые пороги, шкалы баллов, перевод баллов в оценку)
* имеет схемы и проходит валидацию;
* даёт возможность написать **golden tests**.

---

## B1. Организация работ (очень практично)

### Шаг 1 — “сырые” выгрузки

Создать папку:

```
/tools/norms_extraction/
  raw/
  intermediate/
  out/
  README.md
```

В `raw/` складываем:

* `315 полный.pdf`
* (опционально) вручную вырезанные страницы приложений в отдельные PDF: `appendix10.pdf`, `appendix11.pdf` и т.д. (это ускоряет извлечение).

### Шаг 2 — извлечение таблиц в промежуточные CSV

Цель — получить **CSV, где каждая строка = строка таблицы**, без попытки “умничать”.

* `intermediate/appendix10_matrix.csv`
  Колонки: `exercise_id, name, M1..M8, F1..F6`
* `intermediate/appendix11_thresholds.csv`
  Колонки: `sex, age_group, category, n_required, min_points_per_exercise, excellent, good, satisfactory`
* `intermediate/appendix12_scales.csv`
  Колонки: `exercise_id, sex, age_group, variant, result_value, points`
  (если в таблице диапазоны — сохранять как две колонки `result_from, result_to`)
* `intermediate/appendix13_points_to_grade.csv`
  Колонки: `min_points, grade`

> Важно: на этом шаге допускается ручная корректировка CSV (иногда PDF-таблицы “рвутся”).

### Шаг 3 — нормализация и сборка JSON

Пишется конвертер:

* читает CSV,
* валидирует типы,
* собирает JSON по контракту,
* пишет в `out/rfp2015_v1/`.

Правила нормализации:

* `exercise_id` — int, уникален;
* `sex` — строго `"M"`/`"F"`;
* `age_group` — int (1..8 для М, 1..6 для Ж);
* `better_is` задаётся по `result_type` (время — lower, количество — higher);
* единицы: **всё приводить в базовые** (секунды, метры), чтобы не плодить варианты.

### Шаг 4 — JSON Schema + validation gate

Схемы кладём в `schemas/`. В CI/локально:

* если schema validation не проходит → сборка пакета падает.

### Шаг 5 — golden tests (минимум 10–20)

Набор тестов должен проверять ключевое:

* выбор шкалы по `(exercise_id, sex, age_group, variant)`;
* расчёт `points` из `appendix12`;
* итоговая оценка по `appendix11` + проверка `min_points_per_exercise`. 
* перевод “оценка за упражнение” по `appendix13`. 

Формат фикстуры (пример):

```json
{
  "case_id": "ex39_m_g3",
  "sex": "M",
  "age_group": 3,
  "category": 2,
  "n_required": 4,
  "results": [
    { "exercise_id": 39, "raw": "3:30", "normalized_sec": 210, "expected_points": 100 }
  ],
  "expected_total": 100,
  "expected_grade": "satisfactory"
}
```

---

## B2. Что именно извлекаем из PDF (чек-лист)

1. **Приложение №10** — перечень упражнений + применимость по полу/возрасту (матрица +/–). 
2. **Приложение №11** — таблица итоговой оценки по сумме баллов + минимум в каждом упражнении. 
3. **Приложение №12** — таблицы начисления баллов за выполнение упражнений. 
4. **Приложение №13** — перевод баллов за одно упражнение в оценку. 
5. Отдельно зафиксировать блок **поправок к результатам** (как data), даже если UI пока не использует. 

---

# C) Что сказать Cursor/Codex как “лучший next step”

1. **Сначала реализуем доменные модели + загрузчик Norms Pack + schema validation.**
2. Потом — **конвертер PDF→CSV→JSON** в `/tools/norms_extraction/`.
3. Потом — **golden tests**, которые гарантируют, что расчёт не “поплыл”.

---

Если хочешь, я могу прямо следующим сообщением:

* выдать **готовые JSON Schema** для всех 5 файлов (manifest + 4 приложения),
* и “скелет” утилиты-конвертера (псевдокод + структура модулей), чтобы Cursor вставил и дописал детали извлечения.



Ниже — **готовый блок для вставки в `.md`**:

1. **JSON Schema** для `manifest.json` и `appendix10–13.json` (Draft 2020-12),
2. **скелет утилиты-конвертера** `PDF/CSV → Norms Pack` (структура + псевдокоды + формат CSV),
3. **gate-проверки** и шаблон golden-tests.

> Важно: конкретные цифры/таблицы берутся из приложений №10–13 и блока поправок РФП-2015  — в код не зашиваем, только в `docs/norms/...`.

---

````md
# Norms Pack v1: схемы + утилита сборки (для Cursor/Codex)

## 0) Цель
Сделать `docs/norms/rfp2015_v1/` с:
- `manifest.json`
- `appendix10.json` (упражнения + применимость)
- `appendix11.json` (пороги итоговой оценки по сумме баллов + min per exercise)
- `appendix12.json` (шкалы result→points + поправки как data)
- `appendix13.json` (points→grade за одно упражнение)

Приложения №10–13 и блок поправок являются источником данных .

---

## 1) JSON Schema (Draft 2020-12)

### 1.1 `schemas/manifest.schema.json`
```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.local/schemas/manifest.schema.json",
  "type": "object",
  "required": ["norms_id", "title", "source", "created_at", "version", "compatible_app_versions", "files"],
  "additionalProperties": false,
  "properties": {
    "norms_id": { "type": "string", "pattern": "^[a-z0-9_\\-]+$" },
    "title": { "type": "string", "minLength": 1 },
    "source": {
      "type": "object",
      "required": ["document_name", "document_fingerprint"],
      "additionalProperties": false,
      "properties": {
        "document_name": { "type": "string", "minLength": 1 },
        "document_fingerprint": { "type": "string", "pattern": "^sha256:[a-f0-9]{64}$" },
        "extraction_notes": { "type": "string" }
      }
    },
    "created_at": { "type": "string", "format": "date-time" },
    "version": { "type": "string", "pattern": "^[0-9]+\\.[0-9]+\\.[0-9]+$" },
    "compatible_app_versions": {
      "type": "array",
      "minItems": 1,
      "items": { "type": "string", "minLength": 1 }
    },
    "files": {
      "type": "object",
      "required": ["appendix10", "appendix11", "appendix12", "appendix13"],
      "additionalProperties": false,
      "properties": {
        "appendix10": { "type": "string", "pattern": "^appendix10\\.json$" },
        "appendix11": { "type": "string", "pattern": "^appendix11\\.json$" },
        "appendix12": { "type": "string", "pattern": "^appendix12\\.json$" },
        "appendix13": { "type": "string", "pattern": "^appendix13\\.json$" }
      }
    }
  }
}
````

---

### 1.2 `schemas/appendix10.schema.json` (упражнения + применимость)

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.local/schemas/appendix10.schema.json",
  "type": "object",
  "required": ["exercises", "metadata"],
  "additionalProperties": false,
  "properties": {
    "exercises": {
      "type": "array",
      "minItems": 1,
      "items": { "$ref": "#/$defs/exercise" }
    },
    "metadata": {
      "type": "object",
      "required": ["age_groups"],
      "additionalProperties": false,
      "properties": {
        "age_groups": {
          "type": "object",
          "required": ["M", "F"],
          "additionalProperties": false,
          "properties": {
            "M": { "type": "array", "items": { "type": "integer", "minimum": 1, "maximum": 8 }, "minItems": 1 },
            "F": { "type": "array", "items": { "type": "integer", "minimum": 1, "maximum": 6 }, "minItems": 1 }
          }
        }
      }
    }
  },
  "$defs": {
    "exercise": {
      "type": "object",
      "required": ["exercise_id", "name", "section", "result_type", "unit", "is_group", "attempt_policy", "allowed"],
      "additionalProperties": false,
      "properties": {
        "exercise_id": { "type": "integer", "minimum": 1 },
        "name": { "type": "string", "minLength": 1 },
        "section": {
          "type": "string",
          "enum": [
            "lovkost",
            "bystrota",
            "sila",
            "rukopashnyi_boi",
            "vynoslivost",
            "preodolenie_prepyatstvii",
            "plavanie",
            "podrazdelenie",
            "special"
          ]
        },
        "result_type": { "type": "string", "enum": ["time", "reps", "distance", "score", "mixed"] },
        "unit": { "type": "string", "minLength": 1 },
        "is_group": { "type": "boolean" },
        "attempt_policy": { "type": "string", "enum": ["single", "two_if_fall"] },
        "allowed": { "$ref": "#/$defs/allowedMatrix" }
      }
    },
    "allowedMatrix": {
      "type": "object",
      "required": ["M", "F"],
      "additionalProperties": false,
      "properties": {
        "M": { "$ref": "#/$defs/ageMapM" },
        "F": { "$ref": "#/$defs/ageMapF" }
      }
    },
    "ageMapM": {
      "type": "object",
      "additionalProperties": false,
      "required": ["1","2","3","4","5","6","7","8"],
      "properties": {
        "1": { "type": "boolean" }, "2": { "type": "boolean" }, "3": { "type": "boolean" }, "4": { "type": "boolean" },
        "5": { "type": "boolean" }, "6": { "type": "boolean" }, "7": { "type": "boolean" }, "8": { "type": "boolean" }
      }
    },
    "ageMapF": {
      "type": "object",
      "additionalProperties": false,
      "required": ["1","2","3","4","5","6"],
      "properties": {
        "1": { "type": "boolean" }, "2": { "type": "boolean" }, "3": { "type": "boolean" },
        "4": { "type": "boolean" }, "5": { "type": "boolean" }, "6": { "type": "boolean" }
      }
    }
  }
}
```

---

### 1.3 `schemas/appendix11.schema.json` (итоговые пороги)

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.local/schemas/appendix11.schema.json",
  "type": "object",
  "required": ["thresholds"],
  "additionalProperties": false,
  "properties": {
    "thresholds": {
      "type": "array",
      "minItems": 1,
      "items": { "$ref": "#/$defs/thresholdRow" }
    }
  },
  "$defs": {
    "thresholdRow": {
      "type": "object",
      "required": ["sex", "age_group", "category", "n_required", "min_points_per_exercise", "grades"],
      "additionalProperties": false,
      "properties": {
        "sex": { "type": "string", "enum": ["M", "F"] },
        "age_group": { "type": "integer", "minimum": 1, "maximum": 8 },
        "category": { "type": "integer", "enum": [1, 2, 3] },
        "n_required": { "type": "integer", "minimum": 1, "maximum": 8 },
        "min_points_per_exercise": { "type": "integer", "minimum": 0, "maximum": 100 },
        "grades": {
          "type": "object",
          "required": ["excellent", "good", "satisfactory"],
          "additionalProperties": false,
          "properties": {
            "excellent": { "type": "integer", "minimum": 0 },
            "good": { "type": "integer", "minimum": 0 },
            "satisfactory": { "type": "integer", "minimum": 0 }
          }
        }
      }
    }
  }
}
```

---

### 1.4 `schemas/appendix12.schema.json` (шкалы result→points + поправки)

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.local/schemas/appendix12.schema.json",
  "type": "object",
  "required": ["scales", "adjustments"],
  "additionalProperties": false,
  "properties": {
    "scales": {
      "type": "array",
      "minItems": 1,
      "items": { "$ref": "#/$defs/scale" }
    },
    "adjustments": {
      "type": "object",
      "required": ["rules", "max_adjustments_per_result"],
      "additionalProperties": false,
      "properties": {
        "rules": {
          "type": "array",
          "items": { "$ref": "#/$defs/adjustmentRule" }
        },
        "max_adjustments_per_result": { "type": "integer", "minimum": 0, "maximum": 5 }
      }
    }
  },
  "$defs": {
    "scale": {
      "type": "object",
      "required": ["scale_id", "exercise_id", "sex", "age_group", "variant", "result_type", "unit", "better_is", "rows"],
      "additionalProperties": false,
      "properties": {
        "scale_id": { "type": "string", "minLength": 1 },
        "exercise_id": { "type": "integer", "minimum": 1 },
        "sex": { "type": "string", "enum": ["M", "F"] },
        "age_group": { "type": "integer", "minimum": 1, "maximum": 8 },
        "variant": { "type": "string", "minLength": 1 },
        "result_type": { "type": "string", "enum": ["time", "reps", "distance", "score", "mixed"] },
        "unit": { "type": "string", "minLength": 1 },
        "better_is": { "type": "string", "enum": ["lower", "higher"] },
        "rows": {
          "type": "array",
          "minItems": 1,
          "items": { "$ref": "#/$defs/scaleRow" }
        }
      }
    },
    "scaleRow": {
      "type": "object",
      "additionalProperties": false,
      "oneOf": [
        {
          "required": ["result_lte", "points"],
          "properties": {
            "result_lte": { "type": "number" },
            "points": { "type": "integer", "minimum": 0, "maximum": 100 }
          }
        },
        {
          "required": ["result_gte", "points"],
          "properties": {
            "result_gte": { "type": "number" },
            "points": { "type": "integer", "minimum": 0, "maximum": 100 }
          }
        },
        {
          "required": ["result_exact", "points"],
          "properties": {
            "result_exact": { "type": "number" },
            "points": { "type": "integer", "minimum": 0, "maximum": 100 }
          }
        },
        {
          "required": ["result_from", "result_to", "points"],
          "properties": {
            "result_from": { "type": "number" },
            "result_to": { "type": "number" },
            "points": { "type": "integer", "minimum": 0, "maximum": 100 }
          }
        }
      ]
    },
    "adjustmentRule": {
      "type": "object",
      "required": ["adjustment_id", "applies_to", "description", "effect"],
      "additionalProperties": false,
      "properties": {
        "adjustment_id": { "type": "string", "minLength": 1 },
        "applies_to": {
          "type": "array",
          "minItems": 1,
          "items": { "type": "string", "enum": ["time", "distance", "reps", "score", "mixed"] }
        },
        "description": { "type": "string", "minLength": 1 },
        "effect": { "$ref": "#/$defs/effect" }
      }
    },
    "effect": {
      "type": "object",
      "required": ["type", "value"],
      "additionalProperties": false,
      "properties": {
        "type": {
          "type": "string",
          "enum": ["delta_seconds", "delta_meters", "delta_reps", "multiplier"]
        },
        "value": { "type": "number" }
      }
    }
  }
}
```

---

### 1.5 `schemas/appendix13.schema.json` (points→grade за упражнение)

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.local/schemas/appendix13.schema.json",
  "type": "object",
  "required": ["points_to_grade"],
  "additionalProperties": false,
  "properties": {
    "points_to_grade": {
      "type": "array",
      "minItems": 2,
      "items": { "$ref": "#/$defs/row" }
    }
  },
  "$defs": {
    "row": {
      "type": "object",
      "required": ["min_points", "grade"],
      "additionalProperties": false,
      "properties": {
        "min_points": { "type": "integer", "minimum": 0, "maximum": 100 },
        "grade": { "type": "string", "enum": ["unsatisfactory", "satisfactory", "good", "excellent"] }
      }
    }
  }
}
```

---

## 2) Промежуточные CSV (intermediate/) — контракт

### 2.1 `appendix10_matrix.csv`

Колонки:

* `exercise_id` (int)
* `name` (string)
* `section` (lovkost/bystrota/sila/...)
* `result_type` (time/reps/distance/score/mixed)
* `unit` (sec/reps/m/points/...)
* `is_group` (0/1)
* `attempt_policy` (single/two_if_fall)
* `M1..M8` (0/1)
* `F1..F6` (0/1)

### 2.2 `appendix11_thresholds.csv`

Колонки:

* `sex` (M/F)
* `age_group` (int)
* `category` (1/2/3)
* `n_required` (int)
* `min_points_per_exercise` (int)
* `excellent` (int)
* `good` (int)
* `satisfactory` (int)

### 2.3 `appendix12_scales.csv`

Минимально:

* `exercise_id` (int)
* `sex` (M/F)
* `age_group` (int)
* `variant` (base/...)
* `result_type` (time/...)
* `unit` (sec/...)
* `better_is` (lower/higher)
* ОДИН из наборов:

  * `result_lte, points`
  * `result_gte, points`
  * `result_exact, points`
  * `result_from, result_to, points`

### 2.4 `appendix12_adjustments.csv` (если выделяем отдельно)

* `adjustment_id`
* `applies_to` (time|distance|...)
* `description`
* `effect_type` (delta_seconds|...)
* `effect_value` (number)

### 2.5 `appendix13_points_to_grade.csv`

* `min_points` (int)
* `grade` (unsatisfactory|satisfactory|good|excellent)

---

## 3) Утилита сборки Norms Pack (скелет)

### 3.1 Структура

```
tools/norms_extraction/
  README.md
  build_norms_pack.(pas|py|any)   # утилита сборки из CSV в JSON
  validate_norms_pack.(pas|py|any)
  raw/
  intermediate/
  out/
```

### 3.2 Поведение `build_norms_pack`

Вход:

* путь к `intermediate/`
* `--pdf-sha256` (хеш исходного PDF)
* `--norms-id rfp2015_v1`
* `--version 1.0.0`
* `--created-at 2026-02-01T00:00:00Z`

Выход:

* `out/rfp2015_v1/manifest.json`
* `out/rfp2015_v1/appendix10.json` ...
* `out/rfp2015_v1/schemas/*.schema.json` (копия схем)
* `out/rfp2015_v1/pack.sha256` (хеш набора файлов)

Псевдокод:

```text
read CSV appendix10_matrix
  validate columns present
  map rows -> exercises[]
  ensure unique(exercise_id)
  ensure M1..M8 and F1..F6 are booleans
  write appendix10.json

read CSV appendix11_thresholds
  validate required fields
  ensure thresholds unique by (sex, age_group, category, n_required)
  write appendix11.json

read CSV appendix12_scales
  group by (exercise_id, sex, age_group, variant)
  for each group:
    build scale with rows[]
    validate each row has exactly one of [lte,gte,exact,range]
  write appendix12.json (scales)

read CSV appendix12_adjustments (optional)
  build adjustments.rules[]
  write into appendix12.json.adjustments

read CSV appendix13_points_to_grade
  sort by min_points asc
  ensure first min_points == 0
  write appendix13.json

write manifest.json referencing the json files, include pdf fingerprint and notes

compute pack hash:
  sha256(concat of sha256(file_bytes) in deterministic order)
  write pack.sha256
```

### 3.3 Поведение `validate_norms_pack`

Проверки:

1. JSON Schema validation каждого файла.
2. Логические invariants (обязательные):

* `appendix10.exercises[].exercise_id` покрывают все `appendix12.scales[].exercise_id`
* Для каждого scale: rows отсортированы по “строгости” (для time/lower — от лучшего к худшему или наоборот, но единообразно) + нет дублей границ
* `appendix11.thresholds` содержат строки хотя бы для тех комбинаций (sex, age_group, category) которые реально используются в продукте
* `appendix13.points_to_grade` монотонно по min_points

3. Выводит отчёт: ok / список ошибок.

---

## 4) Golden tests (fixtures) — минимальный контракт

Папка:

```
tests/fixtures/rfp2015_v1/
  case_001.json
  ...
```

Формат кейса:

```json
{
  "case_id": "case_001",
  "sex": "M",
  "age_group": 3,
  "category": 2,
  "n_required": 4,
  "items": [
    {
      "exercise_id": 39,
      "raw": "3:30",
      "normalized": { "type": "time", "sec": 210 },
      "adjustments": [],
      "expected_points": 100,
      "expected_grade_for_exercise": "excellent"
    }
  ],
  "expected_total_points": 100,
  "expected_final_grade": "satisfactory"
}
```

Тесты должны гарантировать:

* поиск шкалы по ключу;
* применение (или игнор) поправок в зависимости от политики;
* min_points_per_exercise;
* итог по порогам.

---

## 5) Принципы для Cursor/Codex

* Нормативные данные — только в Norms Pack.
* Код реализует только: загрузку, валидацию, вычисления по данным, аудит.
* Любая «непонятность» в PDF -> фиксируем как issue и отражаем в `extraction_notes` в manifest.

```

---

Если хочешь, я следующим шагом могу **дописать конкретный “политический” флаг** в pack/app:  
- `AdjustmentsPolicy: disabled|enabled` (чтобы в v1 не блокировать релиз),  
и дать **строгое правило сортировки rows** для time/rep/distance, чтобы расчёт был абсолютно детерминированный.
```



````md
# Дополнение к Norms Pack v1: политики поправок + детерминированность шкал

Ниже — обязательные правила/поля, чтобы расчёт **никогда не зависел от “порядка строк”** и чтобы можно было безопасно релизнуть v1 **без включения поправок**, но уже с их хранением как data (источник — блок поправок и приложения РФП-2015). :contentReference[oaicite:0]{index=0}

---

## 1) Политики (Policies) — что включено в расчёт

### 1.1 Где хранить политики
Политики должны храниться:
- **в приложении (config)** — как runtime-флаги поведения;
- **и в сессии проверки** — как snapshot (чтобы расчёт воспроизводился).

**Запрещено**: брать политики “по умолчанию” без фиксации в сессии.

### 1.2 Политики, которые нужны сразу

#### AdjustmentsPolicy (поправки к результатам)
```text
AdjustmentsPolicy:
  disabled  — поправки игнорируются, даже если указаны пользователем
  enabled   — поправки применяются строго по rules из appendix12.adjustments
````

**Рекомендация для v1:** `disabled` по умолчанию, но UI/данные позволяют указать поправки и сохранить их в сессии (как факт), просто они не участвуют в расчёте до включения политики.

#### WomenCategoryPolicy (категория ФП для женщин)

```text
WomenCategoryPolicy:
  force_3   — категория всегда 3, override запрещён
  suggest_3 — категория по умолчанию 3, override разрешён
```

#### RoundingPolicy (округления для времени/дистанции)

Чтобы не было “плавающих” баллов при преобразованиях:

```text
RoundingPolicy:
  time_sec: round_half_up_to_int   (в секундах)
  distance_m: round_half_up_to_1   (в метрах с 1 знаком, если нужно)
  reps: integer_only
```

#### MissingThresholdPolicy (если нет строки в appendix11)

```text
MissingThresholdPolicy:
  error   — расчёт запрещён, показываем ошибку и причину
  fail    — итог FAIL (не рекомендуется)
```

**Рекомендовано:** `error`.

---

## 2) Snapshot политик и Norms Pack внутри Test Session

В БД у сессии проверки хранить:

* `norms_id`
* `norms_pack_hash`
* `policies_snapshot_json` (все политики и их значения)
* `rules_version` (версия приложения/домена)

Минимальный пример `policies_snapshot_json`:

```json
{
  "AdjustmentsPolicy": "disabled",
  "WomenCategoryPolicy": "force_3",
  "RoundingPolicy": {
    "time_sec": "round_half_up_to_int",
    "distance_m": "round_half_up_to_1",
    "reps": "integer_only"
  },
  "MissingThresholdPolicy": "error"
}
```

---

## 3) Детерминированность шкал appendix12 (самое важное)

### 3.1 Общие принципы

При расчёте баллов запрещено:

* полагаться на порядок `rows` в JSON,
* использовать “первое совпадение” без предварительной нормализации шкалы.

**Обязательное правило:** при загрузке Norms Pack домен должен построить `NormalizedScale`, где:

* строки отсортированы канонически,
* выявлены и запрещены пересечения диапазонов,
* выявлены дубли границ,
* сформирована функция `points = f(result)` детерминированно.

### 3.2 Каноническая форма rows

Разрешены 4 формы строк (как в schema):

* `result_lte`
* `result_gte`
* `result_exact`
* `result_from + result_to` (range)

Внутри `NormalizedScale` все строки приводятся к единому типу интервалов:

#### Нормализация

* `result_exact = x` → интервал `[x, x]` (closed)
* `result_lte = x`:

  * для `better_is=lower` (время): интервал `(-∞, x]`
  * для `better_is=higher` (повторы/дист): интервал `[-∞?]` **НЕ допускается** → см. ниже
* `result_gte = x`:

  * для `better_is=higher`: интервал `[x, +∞)`
  * для `better_is=lower`: **не допускается** (нелогично для времени)
* `result_from..result_to` → интервал `[from, to]` (closed)

**Правило запрета:**

* Для `better_is=lower` допускаем только `lte`, `exact`, `range`.
* Для `better_is=higher` допускаем только `gte`, `exact`, `range`.
  Иначе `NORM_PACK_INVALID`.

### 3.3 Сортировка rows (каноническая)

Сортировка должна быть **не по points**, а по границе результата:

#### Для `better_is=lower` (время: меньше — лучше)

Сортируем по `upperBound` по возрастанию:

1. `(-∞, x]` (lte) — по `x`
2. `[from, to]` (range) — по `to`, затем `from`
3. `[x, x]` (exact) — по `x`

#### Для `better_is=higher` (повторы/дистанция: больше — лучше)

Сортируем по `lowerBound` по убыванию:

1. `[x, +∞)` (gte) — по `x` (desc)
2. `[from, to]` (range) — по `from` (desc), затем `to`
3. `[x, x]` (exact) — по `x` (desc)

> Почему exact внизу? Потому что точечные значения — “дыры/исключения”. Их проще проверять отдельно.

---

## 4) Правило выбора points по шкале (единственный источник истины)

Пусть `r` — нормализованный результат (число).

### 4.1 Алгоритм (детерминированный)

1. Найти `scale` по ключу `(exercise_id, sex, age_group, variant)`.

   * Если не найден → `NORM_SCALE_NOT_FOUND`.
2. Применить поправки, если `AdjustmentsPolicy=enabled` (см. раздел 5).
3. Привести `r` к базовой единице и округлить по `RoundingPolicy`.
4. Вычислить points:

**Шаг A — exact**

* Если в шкале есть `exact` для `r`, берём **его** (приоритет exact).

**Шаг B — range**

* Если `r` попадает в диапазон `[from,to]`, берём points этого диапазона.
* Если попадает в несколько диапазонов → `NORM_SCALE_OVERLAP` (запрещено).

**Шаг C — half-infinite**

* Для времени (`better_is=lower`): найти минимальный `x`, такой что `r <= x` (lte). Берём points.
* Для higher: найти максимальный `x`, такой что `r >= x` (gte). Берём points.

**Шаг D — иначе**

* Если не нашли ни exact, ни range, ни half-infinite → `NORM_SCALE_NO_MATCH`
  (это обычно означает “хуже минимального порога” — но фиксируем как ошибку данных/шкалы или как 0 баллов по политике).

### 4.2 Политика “хуже нижнего порога”

Ввести флаг:

```text
OutOfScalePolicy:
  zero_points  — если результата нет в шкале, присваиваем 0
  error        — ошибка расчёта (рекомендовано для QA, но может мешать эксплуатации)
```

**Рекомендация:**

* для prod: `zero_points`
* для tests/CI: `error`

---

## 5) Поправки к результату (Adjustments) — строгие правила

Данные поправок храним в `appendix12.adjustments.rules` (как в schema). 

### 5.1 Ограничения

* Максимум поправок на один результат: `appendix12.adjustments.max_adjustments_per_result` (в документе встречается “до двух”). 
* Поправки применяются только если:

  * `AdjustmentsPolicy=enabled`
  * тип результата входит в `rule.applies_to`

### 5.2 Порядок применения

Порядок должен быть фиксирован:

1. сортируем выбранные поправки по `adjustment_id` (lexicographic)
2. применяем слева направо

### 5.3 Типы effect

* `delta_seconds`: `r := r + value` (для времени; value может быть отрицательным/положительным)
* `delta_meters`: `r := r + value`
* `delta_reps`: `r := r + value`
* `multiplier`: `r := r * value`

После применения поправок выполняем округление по `RoundingPolicy`.

---

## 6) Рекомендованный “режим v1” (чтобы быстро запустить)

### 6.1 Default policies для релиза v1

```json
{
  "AdjustmentsPolicy": "disabled",
  "WomenCategoryPolicy": "force_3",
  "MissingThresholdPolicy": "error",
  "OutOfScalePolicy": "zero_points",
  "RoundingPolicy": {
    "time_sec": "round_half_up_to_int",
    "distance_m": "round_half_up_to_1",
    "reps": "integer_only"
  }
}
```

### 6.2 Что проверить тестами обязательно

* exact приоритетнее range и lte/gte
* перекрывающиеся range → ошибка
* отсутствие шкалы → ошибка
* отсутствие порога appendix11 для комбинации → ошибка (MissingThresholdPolicy=error)
* при `AdjustmentsPolicy=disabled` поправки не меняют points, но сохраняются в raw данных

---

## 7) Мини-API домена (для реализации в Object Pascal)

### 7.1 Интерфейсы (псевдо)

```text
INormsRepository
  Load(norms_id): NormsPack

INormsValidator
  ValidateSchemas(pack): ValidationResult
  ValidateInvariants(pack): ValidationResult

IScaleEngine
  GetPoints(pack, key, raw_result, adjustments, policies): PointsResult
    where key = (exercise_id, sex, age_group, variant)

IGradingEngine
  GetFinalGrade(pack, sex, age_group, category, n_required, item_points[], policies): FinalGradeResult
```

### 7.2 FinalGradeResult

Должен возвращать:

* `final_status`: PASS/FAIL/NO_GRADE/ERROR/NOT_ENOUGH_EXERCISES
* `final_grade`: excellent/good/satisfactory/unsatisfactory/null
* `reason_code`: enum (строго фиксированный)
* `debug`: JSON (порог, суммы, найденные строки)

---

## 8) Reason Codes (минимальный набор)

* `NORM_PACK_INVALID`
* `NORM_SCALE_NOT_FOUND`
* `NORM_SCALE_OVERLAP`
* `NORM_SCALE_NO_MATCH`
* `THRESHOLD_NOT_FOUND`
* `BELOW_MIN_PER_EXERCISE`
* `NOT_ENOUGH_EXERCISES`
* `ADJUSTMENTS_DISABLED`
* `OK`

---

Конец дополнения.

```

Если хочешь — следующим шагом я могу:
- дать **точный контракт `reason_code` + mapping на UI-тексты**,  
- и пример **детерминированного расчёта итоговой оценки** (сортировка упражнений, отбор N_required, поведение при “лишних” упражнениях и при групповых).
```
