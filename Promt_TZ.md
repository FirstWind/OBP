Ты — **ведущий системный аналитик + архитектор ПО + техписатель**. Твоя задача: сгенерировать **профессиональное самодостаточное ТЗ** на разработку desktop-системы учёта и расчёта результатов физической подготовки (РФП-2015 / руководство «315 полный») так, чтобы разработчик **без исходных файлов** реализовал систему **идеально**.

## Главный принцип

* **Никаких догадок.** Если каких-то нормативных цифр/порогов нет в данных — не “восстанавливай”, а требуй загрузку из `Norms Pack` и показывай ошибку при отсутствии.
* Алгоритмы должны быть **детерминированы** и тестируемы.
* Любые “проектные правила” (автокатегория по текстовым полям и т.п.) маркируй как **Derived**, но описывай строго и однозначно.

---

## 1) Цель и границы системы

Система предназначена для:

* ведения справочника личного состава (импорт кадровых выгрузок TSV/CSV/XLSX);
* проведения “сессий проверки”, назначения категории ФП и набора упражнений на каждого человека, ввода результатов;
* детерминированного расчёта баллов по упражнениям и итоговой оценки по сумме баллов (с проверкой минимального порога за упражнение);
* формирования ведомостей/протоколов, экспорт PDF/XLSX;
* аудита (кто/когда/что изменил) и воспроизводимости расчётов (нормативный пакет фиксируется на сессию).

Система НЕ должна:

* требовать интернет/облако;
* быть web-приложением;
* “зашивать” нормативы в код.

---

## 2) Нормативные правила (Confirmed — из 315, реализовать без вариантов)

В ТЗ перечисли и формализуй правила:

### 2.1 Возрастная группа

Возрастная группа определяется по состоянию на **31 декабря года проверки**.

* Мужчины: 1..8 (до 25; 25–29; 30–34; 35–39; 40–44; 45–49; 50–54; 55+)
* Женщины: 1..6 (до 25; 25–29; 30–34; 35–39; 40–44; 45+)

### 2.2 Категории 1/2/3

Категория используется для определения количества упражнений (N) и набора качеств по нормативу.
(Нормативное правило “женщины относятся к кат.3” — учесть как ограничение/политику, см. ниже.)

### 2.3 Сколько упражнений назначается (N) и состав качеств

Зафиксируй нормативное правило назначения N (3/4/5) и требуемых качеств в зависимости от пола/категории/возрастной группы.

### 2.4 Порядок выполнения упражнений

Стандартная последовательность: ловкость → быстрота → сила → рукопашный бой → выносливость → препятствия → служебно-прикладное плавание → упражнения в составе подразделения.
Допускается кастомный порядок “в отдельных случаях”, фиксировать причину.

### 2.5 Попытки

1 попытка. Исключение: для №10/23/24/25/26 при падении со снаряда ещё 1 попытка. Улучшение оценки повтором запрещено.

### 2.6 Групповые упражнения

Оценка подразделения выставляется каждому.

### 2.7 Статусы (отказ/неявка/уважительная причина/медотвод/ЛФК)

Система обязана поддержать статусы участия/результата и влияние на итог:

* `refuse`, `no_show_invalid` ⇒ итог “неудовлетворительно” + причина
* уважительные причины/медотвод/ЛФК ⇒ по умолчанию “нет оценки”, но политика должна быть конфигурируемой админом (например: “перенос/не учитывать/считать неуд.”)

---

## 3) Проектное правило: категория по умолчанию (Derived — реализовать строго как написано ниже)

### 3.1 Где хранится категория

Категория ФП **не относится к персональным данным**, не импортируется и **не хранится в `person`**.
Категория привязывается к участию человека в конкретной сессии проверки.

### 3.2 Нормализация текстов перед поиском

При любом сравнении по текстовым полям (должность/подразделение):

* привести строку к нижнему регистру;
* trim + схлопывание множественных пробелов;
* заменить `ё` → `е`;
* поиск подстрок — case-insensitive по уже нормализованным строкам.

### 3.3 Алгоритм автокатегории (строгий приоритет)

Категория по умолчанию вычисляется **при добавлении участника в сессию** (и по кнопке “пересчитать по умолчанию”, если не было ручного override).

**Шаг 1 — Категория 1 (высший приоритет)**
Если `department` (подразделение) содержит подстроку:

* `группа сопровождения оперативных мероприятий`
  то `category_fp_assigned = 1`.

**Шаг 2 — Категория 2**
Если шаг 1 не выполнен и `position` (должность) содержит любую подстроку:

* `пом.дежур`
* `оперу`
  то `category_fp_assigned = 2`.

**Шаг 3 — Категория 3 по умолчанию**
Иначе `category_fp_assigned = 3`.

### 3.4 Источник категории и ручное переопределение

В данных фиксировать:

* `category_fp_source` = `auto_default` или `manual_override`.

Правило:

* Если пользователь изменил категорию вручную ⇒ `manual_override`.
* Автопересчёт категории **не выполняется** для `manual_override` до нажатия “Сбросить к умолчанию”.

### 3.5 Политика для женщин (выбрать и зафиксировать в ТЗ)

В ТЗ включить настройку (admin policy) `WomenCategoryPolicy`:

* режим `force_3`: женщинам всегда category=3 и поле в UI заблокировано
* режим `suggest_3`: система ставит 3 по умолчанию, но разрешает изменить вручную

Если не задано — по умолчанию `force_3`.

---

## 4) Стек и архитектурные ограничения (ОБЯЗАТЕЛЬНО)

### 4.1 Платформа и режим

* Windows 7/8/10 (x64), без админ-прав.
* Offline-first, LAN допускается.
* Portable или installer (Inno Setup/MSI).

### 4.2 Технологии

* Язык: Object Pascal
* IDE: Lazarus / FreePascal
* UI: LCL (desktop формы)
* DB: Firebird 2.5 или 3.x, файл `.fdb`
* Подключение: classic/server в LAN + (опционально) embedded для single-user

❌ Запрещено: web, cloud, docker, electron, .NET, java, node.

### 4.3 Многопользовательский режим (read/write) — обязателен

Система должна корректно работать в LAN при одновременной работе нескольких пользователей:

* **Единая БД Firebird** на сетевом ресурсе/сервере (classic/superserver) — рекомендовано.
* Поддержка конкурентного доступа:

  * одновременное чтение — без ограничений;
  * одновременное редактирование — через механизм блокировок и транзакций.
* Требования:

  1. Внутри одной сессии проверки редактирование результатов должно быть защищено от “перетирания”.
  2. Должна быть сущность “блокировка” (lock) на уровне сессии и/или участника:

     * кто заблокировал, когда, TTL, причина
  3. Режимы:

     * `draft`: можно редактировать
     * `locked`: только чтение (кроме Admin, если разрешено политикой)
  4. Транзакции:

     * использовать транзакции Firebird
     * продумать уровень изоляции (по умолчанию snapshot)
  5. Конфликты:

     * при попытке изменения заблокированной сущности — понятное сообщение + кто держит lock.

### 4.4 Принцип разделения слоёв

* Domain (rules engine) не зависит от UI и БД.
* Расчёты не в триггерах БД.

---

## 5) Структура проекта (обязательная)

В ТЗ включить референс-структуру:

```
/project-root
  /src
    /app
    /ui
    /ui/viewmodels
    /domain
      /entities
      /services
      /value_objects
      /policies
    /infrastructure
      /db
      /db/migrations
      /import
      /export
      /audit
      /norms
      /locks
    /tests
      /unit
      /fixtures
  /docs
    /tz
    /norms
  /build
  /tools
  README.md
```

---

## 6) Person: импортируемые поля (строго) + системные

В ТЗ зафиксируй: таблица `person` содержит **только импортируемые персональные поля** и служебные поля системы. Категории ФП там нет.

### 6.1 Поля `person` (импорт)

Ключ:

* `personal_no` (Личный номер) — UNIQUE NOT NULL

Импортируемые поля (как строка/дата/булево, допускают NULL):

* `rank`
* `full_name`
* `sex` (M/F; нормализовать)
* `birth_date`
* `position`
* `group`
* `direction`
* `department_unit`
* `department`
* `service`
* `is_command_reserve`
* `position_assigned_date`
* `combat_start_date`
* `combat_end_date`
* `combat_region`
* `reserve_position`
* `dactyl_card_reg_no`
* `employee_category` (используется для фильтра “только военнослужащие”)
* `active_reserve_1`
* `special_attestation_present`
* `agent_admission_order_date`
* `health_group`
* `physical_group`
* `dispensary_date`
* `gb_service_period_start`
* `snils`
* `service_id_1`
* `service_id_2`
* `inn`
* `dismiss_reason`
* `dismiss_date`
* `contract_end_date`

Системные поля:

* `status` ENUM: `active|inactive_commandered|inactive_dismissed|inactive_other`
* `status_changed_at`
* `last_import_id`
* `created_at/by`, `updated_at/by`, `is_deleted`

---

## 7) Norms Pack (нормативы как данные, версионирование)

ТЗ должно описать “Norms Pack” как внешний набор данных:

Папка: `docs/norms/<norms_id>/`

* `manifest.json`
* `appendix10.json` (каталог упражнений + допустимость)
* `appendix11.json` (пороги итоговой оценки для 3/4/5 упражнений + минимум за упражнение)
* `appendix12.json` (шкалы результат→баллы)
* `appendix13.json` (оценка по одному упражнению, если используется)

Обязательно:

* JSON Schema для каждого файла
* валидация целостности
* хранить `norms_id` + `norms_hash` в `test_sessions` и “снимок” порогов в итогах.

---

## 8) Алгоритмы (детерминированно + псевдокод)

### 8.1 Вычисление возрастной группы

Вычислять возраст на дату `31.12.<год(test_session.session_date)>`.
Сохранить `age_group_derived` в участнике (snapshot).

### 8.2 Назначение N_required и качеств (подсказка)

По (sex_snapshot, age_group_derived, category_fp_assigned) получить:

* N_required
* список требуемых качеств/групп упражнений
  Это нормативное правило (Confirmed).

### 8.3 Назначение упражнений (ручное + валидации)

Пользователь выбирает упражнения; система:

* показывает только допустимые (appendix10)
* валидирует наличие требуемых качеств и количество N_required
* сохраняет assignment.

### 8.4 Расчёт баллов по упражнению

Псевдокод:

* нормализовать raw_result
* выбрать шкалу appendix12 по ключу (exercise_id, sex, age_group, variant)
* вычислить points
* сохранить points + нормализованное значение + ссылку на norm_row_id
  Статусы/попытки — по правилам.

### 8.5 Итоговая оценка “в целом”

Псевдокод:

1. обработать статусы участия (refuse/no_show_invalid ⇒ FAIL)
2. уважительные причины ⇒ NO_GRADE (или по политике)
3. проверить наличие результатов минимум для N_required
4. проверить минимум баллов за каждое упражнение (appendix11)
5. total_points = sum(points)
6. grade = по порогам appendix11 (в зависимости от N_required)
7. сохранить `thresholds_snapshot_json`

Ввести исчерпывающий `final_reason_code`:

* OK
* NO_GRADE_VALID_ABSENCE
* FAIL_REFUSE
* FAIL_NO_SHOW_INVALID
* NOT_ENOUGH_EXERCISES
* BELOW_MIN_PER_EXERCISE
* EXERCISE_NOT_ALLOWED
* NORM_PACK_INVALID
* SESSION_LOCKED
* etc.

---

## 9) Импорт кадров (TSV/CSV/XLSX)

ТЗ должно описать:

* авто-детект разделителя TAB/;/,
* авто-детект UTF-8/Win-1251
* фильтрация “только военнослужащие” по `employee_category` содержит “военнослужащ”
* UI-маппинг пресетов колонок
* отчёт импорта (added/updated/skipped/failed)
* аудит импорта

---

## 10) UI/UX (desktop)

Экраны:

* Справочник персонала
* Импорт
* Список сессий
* Сессия: участники, категория (авто + ручной override), назначение упражнений, ввод результатов, итоги
* Отчёты/экспорт
* Админ: norms pack, политики (WomenCategoryPolicy), права, аудит, блокировки

Важное:

* Категория по умолчанию должна объясняться: показать “почему выставлено 1/2/3” (match rule).
* При поиске/матчинге подстрок показывать нормализованные значения (для отладки).

---

## 11) Структура БД Firebird (обязательна) + многопользовательские блокировки

### 11.1 Общие принципы

* PK BIGINT, генераторы/sequence (совместимость 2.5/3.x)
* индексы на ключи поиска
* audit_log на любые изменения
* таблица locks для multiuser

### 11.2 Таблицы (минимум)

#### persons

(как в разделе 6)

#### test_sessions

* id PK
* session_date DATE NOT NULL
* unit_name VARCHAR
* location VARCHAR
* norms_id VARCHAR NOT NULL
* norms_hash VARCHAR NOT NULL
* status VARCHAR(20): draft/active/locked/archived
* order_mode VARCHAR(20): standard/custom
* order_reason VARCHAR

#### session_participants

* id PK
* session_id FK
* person_id FK
* participation_status VARCHAR(30)
* status_reason VARCHAR
* sex_snapshot CHAR(1) NOT NULL
* birth_date_snapshot DATE NOT NULL
* age_group_derived SMALLINT NOT NULL
* category_fp_assigned SMALLINT NOT NULL
* category_fp_source VARCHAR(20) NOT NULL (auto_default/manual_override)
* category_default_reason VARCHAR(200) NULL (например: "department match: ...", "position match: ...")

UNIQUE(session_id, person_id)

#### session_assignments

* id PK
* session_participant_id FK UNIQUE
* n_required SMALLINT NOT NULL
* assignment_mode VARCHAR(20): auto_suggest/manual
* assignment_reason VARCHAR

#### assignment_exercises

* id PK
* session_assignment_id FK
* exercise_id VARCHAR NOT NULL
* variant_id VARCHAR NULL
* quality_group VARCHAR(20) NOT NULL
* sort_order SMALLINT
* is_counted SMALLINT DEFAULT 1

#### attempt_results

* id PK
* assignment_exercise_id FK
* attempt_no SMALLINT NOT NULL
* status VARCHAR(30) NOT NULL
* status_reason VARCHAR
* raw_result_str VARCHAR
* normalized_value DOUBLE PRECISION
* normalized_unit VARCHAR(10)
* points SMALLINT
* norm_row_id VARCHAR
  UNIQUE(assignment_exercise_id, attempt_no)

#### calculated_results

* id PK
* session_participant_id FK UNIQUE
* total_points SMALLINT
* final_grade VARCHAR(10)
* final_reason_code VARCHAR(40) NOT NULL
* calculation_ts TIMESTAMP
* thresholds_snapshot_json BLOB SUB_TYPE TEXT

#### import_batches, mapping_presets, audit_log

(как в прошлой версии, но оставить обязательно)

#### locks (для multiuser)

* id PK
* entity_type VARCHAR(30) NOT NULL (session, participant, assignment)
* entity_id BIGINT NOT NULL
* locked_by VARCHAR(100) NOT NULL
* locked_at TIMESTAMP NOT NULL
* expires_at TIMESTAMP NULL
* reason VARCHAR(200)
  UNIQUE(entity_type, entity_id)

Политика:

* при открытии сессии на редактирование приложение ставит lock (или lock на participant при редактировании строки)
* при закрытии формы/выходе — снимает lock
* при “зависшем” lock — admin может снять вручную (в UI) с записью в audit_log.

### 11.3 Индексы

* persons(personal_no) unique
* persons(full_name)
* test_sessions(session_date)
* session_participants(session_id, person_id) unique
* locks(entity_type, entity_id) unique
* audit_log(ts), audit_log(entity_type, entity_id)

### 11.4 DDL приложение

В ТЗ включить приложение “DDL Firebird” с созданием таблиц+индексов+генераторов/sequence и базовых доменов типов.

---

## 12) Нефункциональные требования

* 50k persons без лагов на поиск
* корректная работа в LAN (concurrency, locks, audit)
* резервное копирование/восстановление
* воспроизводимость результатов (norms pack fixed per session)

---

## 13) Тестирование и приёмка

В ТЗ включить:

* unit tests для:

  * нормализации текста и автокатегории (3 шага + приоритет)
  * вычисления age_group по 31.12
  * расчёта points и финальной оценки
  * конфликтов блокировок (locks)
* 10 эталонных кейсов (fixtures)

---

## 14) Формат вывода

Выведи один Markdown-документ:

# Техническое задание: Desktop-система учёта и расчёта физической подготовки (РФП-2015 / 315)

1. Введение
2. Термины
3. Роли и права
4. Сценарии
5. Архитектура и стек (включая multiuser)
6. Структура проекта
7. Модель данных и БД (ER + DDL приложение)
8. Norms Pack (форматы + JSON Schema)
9. Алгоритмы (псевдокод)
10. Импорт/экспорт
11. UI/валидации
12. НФТ
13. Тестирование/приёмка
14. Риски/TODO

Приложения:

* A: final_reason_code
* B: JSON Schema
* C: DDL Firebird
* D: 10 тест-кейсов

