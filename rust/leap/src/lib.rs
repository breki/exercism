pub fn is_leap_year(year: u64) -> bool {
    let divisible_by_4 = year % 4 == 0;
    return if divisible_by_4 {
        let divisible_by_100 = year % 100 == 0;
        if divisible_by_100 {
            year % 400 == 0
        } else {
            true
        }
    } else {
        false
    };
}
