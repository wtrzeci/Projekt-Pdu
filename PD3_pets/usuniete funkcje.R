ile_odznak <-sqldf::sqldf("SELECT UserId, COUNT(*) as Suma
                            FROM Badges
                            GROUP BY UserId
                            JOIN Users ON Users.AccountId=Posts.OwnerUserId
                            ORDER BY Suma DESC 
                            LIMIT 10
                            ")

ile_odznak <-sqldf::sqldf("SELECT COUNT(*) as Suma, UserId
                            FROM Badges
                            WHERE Badges.Class = 1
                            GROUP BY UserId
                            ")