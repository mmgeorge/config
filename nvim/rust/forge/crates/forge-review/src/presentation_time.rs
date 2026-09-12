use std::time::{SystemTime, UNIX_EPOCH};

struct Timestamp {
    year: i64,
    month: i64,
    day: i64,
    epoch: i64,
}

pub(crate) fn latest_activity<'source>(values: impl IntoIterator<Item = &'source str>) -> String {
    let mut latest: Option<(&str, i64)> = None;
    let mut relative_fallback: Option<(&str, u64)> = None;
    let mut fallback = "";
    for value in values {
        if let Some(timestamp) = parse(value) {
            if latest.is_none_or(|(_, epoch)| timestamp.epoch > epoch) {
                latest = Some((value, timestamp.epoch));
            }
        } else if let Some(days) = relative_days(value) {
            if relative_fallback.is_none_or(|(_, current)| days < current) {
                relative_fallback = Some((value, days));
            }
        } else if value > fallback {
            fallback = value;
        }
    }
    relative(latest.map_or_else(
        || relative_fallback.map_or(fallback, |(value, _)| value),
        |(value, _)| value,
    ))
}

fn relative_days(value: &str) -> Option<u64> {
    let (count, suffix) = value.trim().split_once(' ')?;
    matches!(suffix, "day ago" | "days ago")
        .then(|| count.parse().ok())
        .flatten()
}

pub(crate) fn relative(value: &str) -> String {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |duration| duration.as_secs().min(i64::MAX as u64) as i64);
    relative_at(value, now)
}

fn relative_at(value: &str, now: i64) -> String {
    let value = value.trim();
    let Some(timestamp) = parse(value) else {
        return value.into();
    };
    let seconds = now.saturating_sub(timestamp.epoch);
    if seconds < 60 {
        return "just now".into();
    }
    if seconds < 3600 {
        return ago(seconds / 60, "minute");
    }
    if seconds < 86400 {
        return ago(seconds / 3600, "hour");
    }
    let days = now.div_euclid(86400) - timestamp.epoch.div_euclid(86400);
    match days {
        1 => "Yesterday".into(),
        2..=6 | 14..=29 => ago(days, "day"),
        7..=13 => "Last week".into(),
        30..=59 => "Last month".into(),
        _ => {
            let months = [
                "January",
                "February",
                "March",
                "April",
                "May",
                "June",
                "July",
                "August",
                "September",
                "October",
                "November",
                "December",
            ];
            format!(
                "{} {}, {}",
                months[timestamp.month as usize - 1],
                timestamp.day,
                timestamp.year
            )
        }
    }
}

fn ago(count: i64, unit: &str) -> String {
    format!("{count} {unit}{} ago", if count == 1 { "" } else { "s" })
}

fn parse(value: &str) -> Option<Timestamp> {
    let value = value.trim();
    if value.len() < 19
        || value.get(4..5)? != "-"
        || value.get(7..8)? != "-"
        || !matches!(value.get(10..11)?, "T" | " ")
        || value.get(13..14)? != ":"
        || value.get(16..17)? != ":"
    {
        return None;
    }
    let number = |start, end| value.get(start..end)?.parse::<i64>().ok();
    let year = number(0, 4)?;
    let month = number(5, 7)?;
    let day = number(8, 10)?;
    let hour = number(11, 13)?;
    let minute = number(14, 16)?;
    let second = number(17, 19)?;
    let leap = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
    let month_length = [
        31,
        if leap { 29 } else { 28 },
        31,
        30,
        31,
        30,
        31,
        31,
        30,
        31,
        30,
        31,
    ];
    if !(1..=12).contains(&month)
        || !(1..=month_length[month as usize - 1]).contains(&day)
        || !(0..24).contains(&hour)
        || !(0..60).contains(&minute)
        || !(0..60).contains(&second)
    {
        return None;
    }
    let suffix = value.get(19..)?;
    let zone = if let Some(fraction) = suffix.strip_prefix('.') {
        fraction.trim_start_matches(|character: char| character.is_ascii_digit())
    } else {
        suffix
    };
    let offset = match zone {
        "" | "Z" | "z" => 0,
        _ if zone.len() == 6 && matches!(zone.get(0..1)?, "+" | "-") && zone.get(3..4)? == ":" => {
            let zone_hour = zone.get(1..3)?.parse::<i64>().ok()?;
            let zone_minute = zone.get(4..6)?.parse::<i64>().ok()?;
            if zone_hour > 23 || zone_minute > 59 {
                return None;
            }
            (zone_hour * 3600 + zone_minute * 60) * if zone.starts_with('-') { -1 } else { 1 }
        }
        _ => return None,
    };
    let adjusted_year = year - i64::from(month <= 2);
    let era = adjusted_year.div_euclid(400);
    let year_in_era = adjusted_year - era * 400;
    let adjusted_month = month + if month > 2 { -3 } else { 9 };
    let day_in_year = (153 * adjusted_month + 2) / 5 + day - 1;
    let days = era * 146097 + year_in_era * 365 + year_in_era / 4 - year_in_era / 100 + day_in_year
        - 719468;
    Some(Timestamp {
        year,
        month,
        day,
        epoch: days * 86400 + hour * 3600 + minute * 60 + second - offset,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn relative_dates_preserve_compact_units_and_calendar_boundaries() {
        let now = parse("2026-09-08T19:00:00Z").unwrap().epoch;
        for (source, expected) in [
            ("2026-09-08T19:01:00Z", "just now"),
            ("2026-09-08T18:59:00Z", "1 minute ago"),
            ("2026-09-08T18:58:00Z", "2 minutes ago"),
            ("2026-09-08T18:00:00Z", "1 hour ago"),
            ("2026-09-07T14:00:00Z", "Yesterday"),
            ("2026-09-06T14:00:00Z", "2 days ago"),
            ("2026-09-01T14:00:00Z", "Last week"),
            ("2026-08-15T14:00:00Z", "24 days ago"),
            ("2026-08-01T14:00:00Z", "Last month"),
            ("2026-06-01T14:00:00Z", "June 1, 2026"),
            ("invalid", "invalid"),
        ] {
            assert_eq!(relative_at(source, now), expected, "{source}");
        }
        assert_eq!(parse("2026-09-08T09:00:00-10:00").unwrap().epoch, now);
        assert!(parse("2026-02-29T00:00:00Z").is_none());
        assert!(parse("2024-02-29T00:00:00Z").is_some());
        assert_eq!(latest_activity(["3 days ago", "2 days ago"]), "2 days ago");
    }
}
