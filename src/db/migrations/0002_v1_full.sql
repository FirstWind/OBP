alter table persons add rank varchar(50);
alter table persons add "group" varchar(100);
alter table persons add direction varchar(100);
alter table persons add department_unit varchar(200);
alter table persons add service varchar(200);
alter table persons add is_command_reserve smallint default 0 not null;
alter table persons add position_assigned_date date;
alter table persons add combat_start_date date;
alter table persons add combat_end_date date;
alter table persons add combat_region varchar(200);
alter table persons add reserve_position varchar(200);
alter table persons add dactyl_card_reg_no varchar(50);
alter table persons add active_reserve_1 varchar(50);
alter table persons add special_attestation_present smallint default 0 not null;
alter table persons add agent_admission_order_date date;
alter table persons add health_group varchar(50);
alter table persons add physical_group varchar(50);
alter table persons add dispensary_date date;
alter table persons add gb_service_period_start date;
alter table persons add snils varchar(20);
alter table persons add service_id_1 varchar(50);
alter table persons add service_id_2 varchar(50);
alter table persons add inn varchar(20);
alter table persons add dismiss_reason varchar(200);
alter table persons add dismiss_date date;
alter table persons add contract_end_date date;
alter table persons add status varchar(30);
alter table persons add status_changed_at timestamp;
alter table persons add last_import_id bigint;
alter table persons add created_by varchar(100);
alter table persons add updated_by varchar(100);
alter table persons add is_deleted smallint default 0 not null;

update persons set status = 'active' where status is null;
update persons set is_deleted = 0 where is_deleted is null;
update persons set created_by = 'system' where created_by is null;

alter table persons alter status set not null;
alter table persons alter created_by set not null;
alter table persons add constraint ck_persons_sex check (sex in ('M','F'));
alter table persons add constraint ck_persons_status check (status in ('active','inactive_commandered','inactive_dismissed','inactive_other'));
alter table persons add constraint ck_persons_is_deleted check (is_deleted in (0,1));

create index ix_persons_status on persons(status);

alter table test_sessions add status varchar(20);
update test_sessions set status = 'draft' where status is null;
alter table test_sessions alter status set not null;
alter table test_sessions add constraint ck_test_sessions_status check (status in ('draft','active','locked','archived'));
alter table test_sessions add created_by varchar(100);
update test_sessions set created_by = 'system' where created_by is null;
alter table test_sessions alter created_by set not null;

alter table session_participants add constraint ck_session_participants_category check (category_fp_assigned in (1,2,3));
alter table session_participants add constraint ck_session_participants_category_src check (category_fp_source in ('auto_default','manual_override','policy_force_3'));
alter table session_participants add constraint ck_session_participants_age_med_src check (age_group_med_source in ('manual','medical_commission'));
alter table session_participants add constraint ck_session_participants_sex_snapshot check (sex_snapshot in ('M','F'));
alter table session_participants add constraint ck_session_participants_status check (participation_status in ('completed','refuse','no_show_invalid','no_show_valid','medical_exempt','lfk'));
alter table session_participants add constraint ck_session_participants_reason check (participation_reason_code in ('BUSINESS_TRIP','DUTY','VACATION','SICK_LEAVE','MEDICAL_EXEMPT','LFK','NO_SHOW'));

alter table session_assignments add constraint ck_session_assignments_mode check (assignment_mode in ('auto_suggest','manual'));

alter table assignment_exercises add exercise_id_int integer;
update assignment_exercises
  set exercise_id_int = cast(exercise_id as integer)
where exercise_id similar to '[0-9]+';
alter table assignment_exercises add constraint ck_assignment_exercises_is_counted check (is_counted in (0,1));

alter table assignment_exercises drop exercise_id;
alter table assignment_exercises alter exercise_id_int to exercise_id;

alter table attempt_results add out_of_scale smallint default 0 not null;
alter table attempt_results add out_of_scale_policy varchar(20);
alter table attempt_results add constraint ck_attempt_results_status check (status in ('completed','invalid'));
alter table attempt_results add constraint ck_attempt_results_out_of_scale check (out_of_scale in (0,1));

alter table calculated_results alter thresholds_snapshot_json drop not null;
