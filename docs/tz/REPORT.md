# Отчёт об исправлениях

## Сделано
- Read-only режим: форма стартует в блокировке до первого успешного пинга.
- Монитор соединения уведомляет о потере связи даже при первом неуспешном пинге.
- DDL: nullable-ограничения для `participation_reason_code` и `age_group_med_source`, расширен `attempt_results.status`, добавлены `updated_at/updated_by` в `test_sessions`.
- Миграции приведены в соответствие с DDL.

## Затронутые файлы
- `src/ui/MainForm.pas`
- `src/infrastructure/db/ConnectionMonitor.pas`
- `docs/db/ddl_v1.sql`
- `src/db/migrations/0002_v1_full.sql`

## Тесты
- Не запускались.
