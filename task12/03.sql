-- Выведите столицу Малайзии (Malaysia) (в выводе: только название города).
-- (0,5 баллов)
SELECT City.Name
  FROM Country JOIN Capital, City ON Capital.CountryCode = Country.Code AND City.Id = Capital.CityId
  WHERE Country.Name = "Malaysia" AND City.Id = Capital.CityId 
