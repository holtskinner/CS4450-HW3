module HomeworkSols (
  prob1,
  prob2,
  prob3,
  prob4,
  prob5
  ) where

prob1 :: Char -> Char
prob1 'z' = 'a'
prob1 'Z' = 'A'
prob1 a
      | a >= 'a' && a < 'z' = succ a
      | a >= 'A' && a < 'Z' = succ a
      | otherwise           = a

prob2 :: Char -> Int
prob2 '0' = 0
prob2 '1' = 1
prob2 '2' = 2
prob2 '3' = 3
prob2 '4' = 4
prob2 '5' = 5
prob2 '6' = 6
prob2 '7' = 7
prob2 '8' = 8
prob2 '9' = 9
prob2 _   = -1

prob3 :: (a -> b) -> (a -> c) -> a -> (b, c)
prob3 f g x = (f x, g x)

prob4 :: Bool -> a -> a -> a
prob4 True  a _ = a
prob4 False _ a = a

prob5 :: Integer -> Bool
prob5 y = if (y `mod` 4 /= 0)
          then False
          else (if (y `mod` 100 /= 0)
                then True
                else (if (y `mod` 400 /= 0)
                      then False
                      else True))

prob1Test1 = [prob1 x | x <- (['a'..'z'] ++ "0123456789")] == ['b'..'z'] ++ "a0123456789"
prob1Test2 = [prob1 x | x <- (['A'..'Z'] ++ "0123456789")] == ['B'..'Z'] ++ "A0123456789"
prob2Test1 = [prob2 x |x <- ['0'..'9']] == [0,1,2,3,4,5,6,7,8,9]
prob2Test2 = let res = [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
              in [prob2 x |x <- (['A'..'Z'] ++ ['\n','\r',' '])] == res
prob3Test1 = let res = [('0',0),('1',1),('2',2),('3',3),('A',-1),('B',-1),('C',-1)]
                 in [prob3 prob1 prob2 x | x <- "0123ZAB"] == res
prob3Test2 = [prob3 (prob2 . prob1) prob2 x | x <- "0123"] == [(0,0),(1,1),(2,2),(3,3)]
prob4Test1 = [(\x -> prob4 x 'a' 'b') y|y <- (take 10 $ cycle [True,False])] == "ababababab"
prob4Test2 = [(\x -> prob4 x 'b' 'a') y|y <- (take 10 $ cycle [True,False])] == "bababababa"
prob5Test1 = let leapYears = [1804, 1808, 1812, 1816, 1820, 1824, 1828, 1832, 1836, 1840, 1844, 1848, 1852, 1856, 1860, 1864, 1868, 1872, 1876, 1880, 1884, 1888, 1892, 1896, 1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 1940, 1944, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016] 
 in [ x | x <- leapYears, prob5 x] == leapYears
prob5Test2 = let commonYears = [1800,1801,1802,1803,1805,1806,1807,1809,1810,1811,1813,1814,1815,1817,1818,1819,1821,1822,1823,1825,1826,1827,1829,1830,1831,1833,1834,1835,1837,1838,1839,1841,1842,1843,1845,1846,1847,1849,1850,1851,1853,1854,1855,1857,1858,1859,1861,1862,1863,1865,1866,1867,1869,1870,1871,1873,1874,1875,1877,1878,1879,1881,1882,1883,1885,1886,1887,1889,1890,1891,1893,1894,1895,1897,1898,1899,1900,1901,1902,1903,1905,1906,1907,1909,1910,1911,1913,1914,1915,1917,1918,1919,1921,1922,1923,1925,1926,1927,1929,1930,1931,1933,1934,1935,1937,1938,1939,1941,1942,1943,1945,1946,1947,1949,1950,1951,1953,1954,1955,1957,1958,1959,1961,1962,1963,1965,1966,1967,1969,1970,1971,1973,1974,1975,1977,1978,1979,1981,1982,1983,1985,1986,1987,1989,1990,1991,1993,1994,1995,1997,1998,1999,2001,2002,2003,2005,2006,2007,2009,2010,2011,2013,2014,2015,2017]
  in [ x | x <- commonYears, (not . prob5) x] == commonYears



testResults = let probs = ["prob1Test1","prob1Test2","prob2Test1","prob2Test2","prob3Test1","prob3Test2","prob4Test1","prob4Test2","prob5Test1","prob5Test2"]
                  tests = [prob1Test1,prob1Test2,prob2Test1,prob2Test2,prob3Test1,prob3Test2,prob4Test1,prob4Test2,prob5Test1,prob5Test2]
                 in zip probs tests

calculateGrades = map (\x -> if (snd x == True) then (fst x ++ " -- Passed: ",5) else (fst x ++ " -- Failed: ",2)) testResults

grade = sum $ map snd calculateGrades

printResults []         = do putStrLn ("Score: " ++ show grade)
printResults ((x,y):xs) = do putStrLn (x ++ show y)
                             printResults xs


                     
