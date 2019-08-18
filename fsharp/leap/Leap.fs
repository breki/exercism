module Leap

let divisible year by = year % by = 0

let leapYear (year: int): bool = 
    divisible year 4 
    && (not (divisible year 100) || divisible year 400)