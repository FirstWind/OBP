# OBP (РФП‑2015 / «315 полный»)

Desktop‑система учёта и расчёта физподготовки для Windows (Lazarus/FPC + Firebird 3.x).

## Требования
- Windows 7/8/10 x64
- Lazarus / FreePascal 3.2.x
- Firebird Server 3.x (LAN)
- Git (для подтягивания submodule)

## Быстрый старт
1) Инициализировать submodule:
```
git submodule update --init --recursive
```

2) Установить Firebird Server 3.x и создать БД:
```
isql -user SYSDBA -password masterkey -i docs\db\ddl_v1.sql
```
Путь к БД задаётся в `src/app/config/app.ini`.

3) Указать параметры подключения в `src/app/config/app.ini`:
```
[db]
host=127.0.0.1
port=3050
database=C:\data\obp.fdb
user=SYSDBA
password=masterkey
```

4) Сборка и тесты:
```
tools\run_tests.ps1
tools\build_app.ps1
```

5) Запуск:
```
src\app\OBP.exe
```

## Norms Pack
Нормативные данные должны находиться в папке `docs/norms/<norms_id>/`.
Пример: `docs/norms/rfp2015_v1`.

В UI указывайте путь к папке Norms Pack (нужны `manifest.json` и `appendix10–13.json`).

## Отчёты и XLSX
Экспорт в XLSX реализован через `fpSpreadsheet` (submodule `third_party/fpspreadsheet`).
В проекте используется совместимая версия `xlsxooxml.pas` в `src/compat_fps`.

## Примечания
- При потере соединения UI блокируется (read‑only) и показывает статус подключения.
- Создатель/актор берётся из Windows‑логина пользователя (переменная `USERNAME`).
