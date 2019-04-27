-- Old
create table midiport (
       port_id    INTEGER PRIMARY KEY NOT NULL,
       port_name  TEXT    NOT NULL
);
create table midiclock (
       port_name  TEXT    PRIMARY KEY NOT NULL
);
create view midiclock_mask as
select sum(1<<port_id) from midiclock left join midiport on midiclock.port_name = midiport.port_name;

-- New
-- Simpler? Just record all the connections and aliases?
-- create table connect (a TEXT NOT NULL, b TEXT NOT NULL);

