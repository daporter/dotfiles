#!/usr/bin/env bash
# Claude Code status line: model, context usage bar, session cost/duration, rate limits.

input=$(cat)

dim=$'\033[2m'
reset=$'\033[0m'
sep=" ${dim}·${reset} "

# Color by severity: green < 50, yellow < 80, red >= 80.
color_for_pct() {
	local pct=$1
	if [ "$pct" -ge 80 ]; then
		printf 31 # red
	elif [ "$pct" -ge 50 ]; then
		printf 33 # yellow
	else
		printf 32 # green
	fi
}

format_duration() {
	local total_s=$(($1 / 1000))
	local h=$((total_s / 3600))
	local m=$(((total_s % 3600) / 60))
	local s=$((total_s % 60))
	if [ "$h" -gt 0 ]; then
		printf '%dh%02dm' "$h" "$m"
	elif [ "$m" -gt 0 ]; then
		printf '%dm' "$m"
	else
		printf '%ds' "$s"
	fi
}

model=$(printf '%s' "$input" | jq -r '.model.display_name // "unknown"')
used=$(printf '%s' "$input" | jq -r '.context_window.used_percentage // empty')
cost_usd=$(printf '%s' "$input" | jq -r '.cost.total_cost_usd // empty')
duration_ms=$(printf '%s' "$input" | jq -r '.cost.total_duration_ms // empty')
five_hour=$(printf '%s' "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
seven_day=$(printf '%s' "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')

out="\033[36m${model}\033[0m"

width=10
if [ -n "$used" ]; then
	pct=$(printf '%.0f' "$used")

	filled=$((pct * width / 100))
	if [ "$filled" -gt "$width" ]; then
		filled=$width
	fi
	empty=$((width - filled))

	color=$(color_for_pct "$pct")

	bar=$(printf '%*s' "$filled" '')
	bar=${bar// /█}
	gap=$(printf '%*s' "$empty" '')
	gap=${gap// /░}

	out+="${sep}\033[${color}m[${bar}${gap}] ${pct}%\033[0m"
fi

if [ -n "$cost_usd" ] || [ -n "$duration_ms" ]; then
	segment=""
	if [ -n "$cost_usd" ]; then
		segment+=$(printf '$%.2f' "$cost_usd")
	fi
	if [ -n "$duration_ms" ]; then
		[ -n "$segment" ] && segment+=" "
		segment+=$(format_duration "$duration_ms")
	fi
	out+="${sep}${segment}"
fi

if [ -n "$five_hour" ] || [ -n "$seven_day" ]; then
	segment=""
	if [ -n "$five_hour" ]; then
		pct5=$(printf '%.0f' "$five_hour")
		color5=$(color_for_pct "$pct5")
		segment+=$(printf '\033[%sm5h %d%%\033[0m' "$color5" "$pct5")
	fi
	if [ -n "$seven_day" ]; then
		pct7=$(printf '%.0f' "$seven_day")
		color7=$(color_for_pct "$pct7")
		[ -n "$segment" ] && segment+=" "
		segment+=$(printf '\033[%sm7d %d%%\033[0m' "$color7" "$pct7")
	fi
	out+="${sep}${segment}"
fi

printf '%b' "$out"
