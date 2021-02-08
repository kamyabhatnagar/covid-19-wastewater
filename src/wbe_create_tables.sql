

CREATE TABLE  IF NOT EXISTS [Polygon] (
  [ID] char NOT NULL PRIMARY KEY,
  [name] char,
  [pop] integer,
  [type] char,
  [wkt] char,
  [file] blob null default (x''),
  [link] char
);

CREATE TABLE IF NOT EXISTS [Reporter] (
  [ID] char NOT NULL PRIMARY KEY,
  [site.IDDefault] char,
  [lab.IDDefault]  char,
  [contactName] char,
  [contactEmail] char,
  [contactPhone] int,
  [allowAccessToSelf] INTEGER,
  [allowAccessToFederalPublicHealthAuthorities] INTEGER,
  [allowAccessToLocalPublicHealthAuthorities] INTEGER,
  [allowAccessToProvinicialPublicHealthAuthorities] INTEGER,
  [allowAccessToOtherDataProviders] INTEGER,
  [allowAccessToAllOrganizations] INTEGER,
  [allowAccessToPublic] INTEGER,
  [allowAccessToSpec] char,
  [notes] char
);

CREATE TABLE  IF NOT EXISTS [Site] (
  [ID] char NOT NULL PRIMARY KEY,
  [name] char,
  [description] char,
  [reporter.ID] char,
  [type] char,
  [typeOther] char,
  [accessType] char,
  [accessTypeOther] char,
  [sample.typeDefault] char,
  [sample.typeOtherDefault] char,
  [sample.collectionDefault] char,
  [sample.collectOtherDefault] char,
  [measurement.fractionAnalysedDefault] char,
  [sample.tempCdefault] char,
  [geoLat] float,
  [geoLong] float,
  [notes] char,
  [Polygon.ID] char,
  [sewerNetworkFileLink] char,
  [sewerNetworkFileBlob]  blob null default (x''),
  FOREIGN KEY ([reporter.ID]) REFERENCES Reporter(ID) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE  IF NOT EXISTS [Sample] (
  [ID] char NOT NULL PRIMARY KEY,
  [site.ID] char,
  [dateTime] dateTime,
  [dateTimeStart] dateTime,
  [dateTimeEnd] dateTime,
  [type] char,
  [typeOther] char,
  [collection] char,
  [collectionOther] char,
  [collectionTriggerTime] float,
  [preTreatment] INTEGER,
  [preTreatmentDescription] char,
  [childID] char,
  [parentID ] char,
  [sizeL] float,
  [samplingTempC] float,
  [storageTempC] float,
  [mailedOnIce] INTEGER,
  [qualityFlag] INTEGER,
  [notes] char,
  FOREIGN KEY ([site.ID]) REFERENCES Site(ID) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE  IF NOT EXISTS [AssayMethod] (
  [ID] char NOT NULL PRIMARY KEY,
  [version] char,
  [sampleSizeL] float,
  [loq] float,
  [lod] float,
  [units] char,
  [unitsOther] char,
  [concentrationMethod]char,
  [extractionMethod]char,
  [pcrMethod]char,
  [qualityAssuranceQC]char,
  [Inhibition] char,
  [surrogateRecovery] char,
  [assayDesc] char,
  [assayDate] dateTime
);



CREATE TABLE  IF NOT EXISTS [Lab] (
  [ID] char NOT NULL PRIMARY KEY,
  [assay.IDDefault] char,
  [laboratoryName] char,
  [contactName] char,
  [contactEmail] char,
  [contactPhone] int,
  [labUpdateDate] date,
  FOREIGN KEY ([assay.IDDefault]) REFERENCES AssayMethod(ID) DEFERRABLE INITIALLY DEFERRED
);



CREATE TABLE  IF NOT EXISTS [CovidPublicHealthData] (
  [ID] char NOT NULL PRIMARY KEY,
  [reporter.ID]char,
  [polygon.ID]char,
  [date] date,
  [dateType] char,
  [valueType] char,
  [Value] float,
  [notes] char,
  FOREIGN KEY ([reporter.ID]) REFERENCES Reporter(ID) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY ([polygon.ID]) REFERENCES Polygon(ID) DEFERRABLE INITIALLY DEFERRED
);


CREATE TABLE  IF NOT EXISTS [Lookups](
  [tableName] char,
  [columnName] char,
  [value] char,
  [description] char
);

CREATE TABLE  IF NOT EXISTS [Measurement] (
  [uID] char NOT NULL PRIMARY KEY,
  [ID] char,
  [sample.ID] char,
  [lab.ID] char,
  [assay.ID] char,
  [analysisDate] date,
  [reportDate] date,
  [fractionAnalysed] char,
  [category] char,
  [categoryOther] char,
  [unit] char,
  [unitOther] char,
  [aggregation] char,
  [aggregationeOther] char,
  [index] char,
  [value] float,
  [qualityFlag] INTEGER,
  [notes] char,
  FOREIGN KEY ([sample.ID]) REFERENCES Sample(ID) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY ([lab.ID]) REFERENCES Lab(ID) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY ([assay.ID]) REFERENCES AssayMethod(ID) DEFERRABLE INITIALLY DEFERRED
)
