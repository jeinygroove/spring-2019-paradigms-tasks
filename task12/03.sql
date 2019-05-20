-- Выведите столицу Малайзии (Malaysia) (в выводе: только название города).
-- (0,5 баллов)
SELECT City.Name
  FROM Capital
  JOIN Country ON Capital.CountryCode = Country.Code
  JOIN City ON Capital.CityId = City.Id
  WHERE Country.Name = "Malaysia"; 
