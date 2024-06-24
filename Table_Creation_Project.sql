create table Tipificacion
(
	Cod_Tipificacion				int				not null ,
	Tipificacion_agente				varchar(80)		not null,
)
go
alter table Tipificacion add	constraint pk_Tipificacion_Cod_Tipificacion primary key (Cod_Tipificacion)	
go

create table Agente
(
	Agente_id					int				not null,
	Nombre_Agente				varchar(80)		not null,
	Fecha_Ingreso_Agente		date			not null,
	Años_Antiguedad				decimal(4,2)	not null,
	Rango_de_Antiguedad			varchar(20)		not null,
	Capacitacion				bit				not null,
	Evaluador_Id				int				not null,
)

go
alter table Agente add constraint pk_Agente_Agente_id primary key (Agente_id)	
alter table Agente add constraint ck_Agente_Fecha_Ingreso_Agente check (Fecha_Ingreso_Agente <= getdate())
alter table Agente add constraint fk_Agente_Evaluador_Id foreign key (Evaluador_Id) references Evaluador(Evaluador_Id)
alter table Agente add constraint fk_Agente_Rango_de_Antiguedad foreign key (Rango_de_Antiguedad) references Rango_Antiguedad(Rango_de_Antiguedad)
go
create table FCR
(
	Cod_FCR						CHAR(4)			not null ,
	First_contact_Resolution	varchar(20)		not null,
)
go
alter table FCR add	constraint pk_FCR_Cod_FCR primary key (Cod_FCR)	
alter table FCR add constraint ck_FCR_Cod_FCR check (Cod_FCR like 'F&%[0-9]%')
go


CREATE TABLE Evaluador
(
	Evaluador_Id		int 			not null,
	Evaluador_Nombre	varchar (80)	not null,
	Cod_Cargo_Evaluador varchar	(10)	not null,
	Cargo_Evaluador		varchar (50)	not null,
)
go
alter table Evaluador add constraint pk_Evaluador primary key (Evaluador_Id)


CREATE TABLE Rango_Antiguedad
(
	Cod_Rango			char (2)	   not null,
	Rango_de_Antiguedad	varchar(20)		not null,
 )
 go
 alter table Rango_Antiguedad add constraint pk_Rango_Antiguedad_Rango_de_Antiguedad primary key (Rango_de_Antiguedad)
 alter table Rango_Antiguedad add constraint ck_Rango_Antiguedad_Cod_Rango check (Cod_Rango = '01' or Cod_Rango = '02' or Cod_Rango = '03')
 go


create table Capacitacion
(
	Capacitacion					bit				not null,
	Descripcion_Capacitacion		varchar(20)		not null
)
alter table Capacitacion
add constraint pk_Capacitacion primary key (Capacitacion)
go


create table Recuento_FCR_x_Antiguedad
(
	Cod_FCR							char(4)			not null,
	Rango_de_Antiguedad				varchar(20)		not null,
	Cantidad_de_Evaluaciones		int	
)
alter table Recuento_FCR_x_Antiguedad
add constraint fk_Recuento_FCR_x_Antiguedad1 foreign key (Rango_de_Antiguedad) references Rango_Antiguedad,
    constraint fk_Recuento_FCR_x_Antiguedad2 foreign key (Cod_FCR) references FCR
go


create table Recuento_FCR_x_Capacitacion
(
	Cod_FCR							char(4)			not null,
	Capacitacion					bit				not null,
	Cantidad_de_Evaluaciones		int	
)
alter table Recuento_FCR_x_Capacitacion
add constraint fk_Recuento_FCR_x_Capacitacion1 foreign key (Capacitacion) references Capacitacion,
    constraint fk_Recuento_FCR_x_Capacitacion2 foreign key (Cod_FCR) references FCR
go


create table Dim_Tiempo
(
	Date_							date			not null,
	anio							int				not null,
	mes								int				not null,
	anio_mes						char(7)			not null,			
	dia								int				not null,
	mes_nombre						varchar(20)		not null,
	dia_semana_nombre				varchar(20)		not null,
	trimestre						int				not null,
	offset_mes_actual				int				not null,
)
alter table Dim_Tiempo
add constraint pk_Dim_Tiempo primary key (Date_)
go


create table Tabla_Llamadas
(
	Id_Llamada					int				not null,
	Agente_id					int				not null,
	Nombre_Agente				varchar(80)		not null,
	Evaluador_Id				int				not null,
	Evaluador_Nombre			varchar(80)		not null,
	Cod_Cargo_Evaluador			varchar(10)		not null,
	Cargo_Evaluador				varchar(50)		not null,
	Dia_Evaluacion				date			not null,
	Fecha_Contacto				date			not null,
	Ejecucion					decimal(3,2)	not null, 
	Nivel_Servicio				decimal(3,2)	not null,
	Cod_FCR						CHAR(4)			not null,
	First_contact_Resolution	varchar(20)		not null,
	Sub_FCR						varchar(15)		not null,
	Fecha_Ingreso_Agente		date			not null,
	Años_Antiguedad				decimal(4,2)	not null,
	Rango_de_Antiguedad			varchar(20)		not null,
	Cantidad					int				not null,
	NmonitoreoXcontacto			varchar(50)		not null,
	Cod_Tipificacion			int				not null,
	Tipificacion_Agente			varchar(80)		not null,
	Subtipificacion_agente		varchar(80)		null,
	Cod_Rango					char(2)			not null,
	Capacitacion				bit				not null,
)

alter table Tabla_Llamadas add constraint pk_Tabla_Llamadas_Id_Llamada primary key (Id_Llamada)	
alter table Tabla_Llamadas add constraint ck_Tabla_Llamadas_Cod_FCR check (Cod_FCR like 'F&%[0-9]%')
alter table Tabla_Llamadas add constraint ck_Tabla_Llamadas_Cod_Rango check (Cod_Rango = '01' or Cod_Rango = '02' or Cod_Rango = '03')
alter table Tabla_Llamadas add constraint ck_Tabla_Llamadas_Dia_Evaluacion check (Dia_Evaluacion <= getdate())
alter table Tabla_Llamadas add constraint ck_Tabla_Llamadas_Fecha_Contacto check (Fecha_Contacto <= getdate())
alter table Tabla_Llamadas add constraint ck_Tabla_Llamadas_Fecha_Ingreso_Agente check (Fecha_Ingreso_Agente <= getdate())
alter table Tabla_Llamadas add	constraint ck_Tabla_Llamadas_Ejecucion check (Ejecucion between 0.00 and 1.00)
alter table Tabla_Llamadas add	constraint ck_Tabla_Llamadas_Nivel_Servicio check (Nivel_Servicio between 0.00 and 1.00)

alter table Tabla_Llamadas add constraint fk_Tabla_Llamadas_Agente_id foreign key (Agente_id) references Agente(Agente_id)
alter table Tabla_Llamadas add constraint fk_Tabla_Llamadas_Evaluador_Id foreign key (Evaluador_Id) references Evaluador(Evaluador_Id)
alter table Tabla_Llamadas add constraint fk_Tabla_Llamadas_Cod_FCR foreign key (Cod_FCR) references FCR(Cod_FCR)
alter table Tabla_Llamadas add constraint fk_Tabla_Llamadas_Cod_Tipificacion foreign key (Cod_Tipificacion) references Tipificacion(Cod_Tipificacion)
alter table Tabla_Llamadas add constraint fk_Tabla_Llamadas_Cod_Rango foreign key (Rango_de_Antiguedad) references Rango_Antiguedad(Rango_de_Antiguedad)
alter table Tabla_Llamadas add constraint fk_Tabla_Llamadas_Dia_Evaluacion foreign key (Dia_Evaluacion) references Dim_Tiempo(Date_)

exec sp_helpconstraint Tabla_Llamadas

