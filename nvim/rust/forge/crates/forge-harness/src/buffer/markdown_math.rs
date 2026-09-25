fn fence_marker(line: &str) -> Option<(char, usize)> {
    let trimmed = line.trim_start();
    let marker = trimmed.chars().next()?;
    if marker != '`' && marker != '~' {
        return None;
    }
    let width = trimmed
        .chars()
        .take_while(|character| *character == marker)
        .count();
    (width >= 3).then_some((marker, width))
}

fn normalize_inline(line: &str) -> String {
    let mut normalized = String::with_capacity(line.len());
    let mut remaining = line;
    loop {
        let opening = [(r"\(", r"\)", "$"), (r"\[", r"\]", "$$")]
            .into_iter()
            .filter_map(|(opening, closing, delimiter)| {
                remaining
                    .find(opening)
                    .map(|position| (position, opening, closing, delimiter))
            })
            .min_by_key(|(position, _, _, _)| *position);
        let Some((position, opening, closing, delimiter)) = opening else {
            normalized.push_str(remaining);
            break;
        };
        let content_start = position + opening.len();
        let Some(relative_end) = remaining[content_start..].find(closing) else {
            normalized.push_str(remaining);
            break;
        };
        let content_end = content_start + relative_end;
        normalized.push_str(&remaining[..position]);
        normalized.push_str(delimiter);
        normalized.push_str(&remaining[content_start..content_end]);
        normalized.push_str(delimiter);
        remaining = &remaining[content_end + closing.len()..];
    }
    normalized
}

pub(super) fn normalize(source: &str) -> String {
    let line_list: Vec<_> = source.split('\n').collect();
    let mut normalized = Vec::with_capacity(line_list.len());
    let mut fence = None;
    let mut line_index = 0;
    while line_index < line_list.len() {
        let line = line_list[line_index];
        if let Some((marker, width)) = fence {
            normalized.push(line.to_owned());
            if let Some((closing, closing_width)) = fence_marker(line)
                && closing == marker
                && closing_width >= width
                && line
                    .trim_start()
                    .chars()
                    .skip(closing_width)
                    .all(char::is_whitespace)
            {
                fence = None;
            }
            line_index += 1;
            continue;
        }
        if let Some(marker) = fence_marker(line) {
            fence = Some(marker);
            normalized.push(line.to_owned());
            line_index += 1;
            continue;
        }

        let closing = match line.trim() {
            r"\[" => Some(r"\]"),
            "$$" => Some("$$"),
            _ => None,
        };
        if let Some(closing) = closing {
            let end = ((line_index + 1)..line_list.len()).find(|index| {
                let candidate = line_list[*index];
                candidate.trim() == closing || fence_marker(candidate).is_some()
            });
            if let Some(end) = end
                && line_list[end].trim() == closing
            {
                let content = line_list[line_index + 1..end]
                    .iter()
                    .map(|part| part.trim())
                    .filter(|part| !part.is_empty())
                    .collect::<Vec<_>>()
                    .join(" ");
                let indent = &line[..line.len() - line.trim_start().len()];
                normalized.push(format!("{indent}\\[\n{indent}{content}\n{indent}\\]"));
                line_index = end + 1;
                continue;
            }
        }
        let trimmed = line.trim();
        let display = trimmed
            .strip_prefix("$$")
            .and_then(|content| content.strip_suffix("$$"))
            .or_else(|| {
                trimmed
                    .strip_prefix(r"\[")
                    .and_then(|content| content.strip_suffix(r"\]"))
            });
        if let Some(content) = display {
            let indent = &line[..line.len() - line.trim_start().len()];
            normalized.push(format!(
                "{indent}\\[\n{indent}{}\n{indent}\\]",
                content.trim()
            ));
        } else {
            normalized.push(normalize_inline(line));
        }
        line_index += 1;
    }
    normalized.join("\n")
}

#[cfg(test)]
mod test {
    use super::normalize;

    #[test]
    fn normalizes_complete_latex_without_changing_code_or_incomplete_blocks() {
        let source = concat!(
            "The equation is:\n\n",
            "\\[\nL_o(x,\\omega_o)\n=\nL_e(x,\\omega_o) + \\int_{\\Omega} L_i\n\\]\n",
            "Where \\(L_o\\) meets \\(L_i\\).\n",
            "A short \\[x + y\\] form.\n",
            "```latex\n\\(literal\\)\n\\[\nliteral\n\\]\n```\n",
            "\\[\nunfinished",
        );
        let expected = concat!(
            "The equation is:\n\n",
            "\\[\nL_o(x,\\omega_o) = L_e(x,\\omega_o) + \\int_{\\Omega} L_i\n\\]\n",
            "Where $L_o$ meets $L_i$.\n",
            "A short $$x + y$$ form.\n",
            "```latex\n\\(literal\\)\n\\[\nliteral\n\\]\n```\n",
            "\\[\nunfinished",
        );
        assert_eq!(normalize(source), expected);
    }
}
