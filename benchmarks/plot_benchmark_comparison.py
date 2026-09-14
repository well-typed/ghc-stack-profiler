#!/usr/bin/env python
# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "matplotlib",
#     "pyqt6",
#     "numpy",
# ]
# ///

"""
This script is an edited version of the Hyperfine benchmark comparison script,
which adds support for the "command_name" and "command_color" keys.

See: https://github.com/sharkdp/hyperfine/blob/master/scripts/plot_benchmark_comparison.py
"""

import argparse
import functools
import json
import pathlib

import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import numpy as np

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("files", nargs="+", type=pathlib.Path, help="JSON files with benchmark results")
parser.add_argument("--title", help="Plot Title")
parser.add_argument("--benchmark-names", nargs="+", help="Names of the benchmark groups")
parser.add_argument("--baseline", help="Compare all benchmarks to the given baseline")
parser.add_argument("-o", "--output", help="Save image to the given filename")

args = parser.parse_args()

commands = None
command_kwargs = None
timing_baseline = None
data = []
inputs = []

if args.benchmark_names:
    assert len(args.files) == len(
        args.benchmark_names
    ), "Number of benchmark names must match the number of input files."

for i, filename in enumerate(args.files):
    with open(filename) as f:
        results = json.load(f)["results"]
    benchmark_commands = [b.get("command_name", b["command"]) for b in results]
    if commands is None:
        commands = benchmark_commands
    else:
        assert (
            commands == benchmark_commands
        ), f"Unexpected commands in {filename}: {benchmark_commands}, expected: {commands}"
    benchmark_kwargs = [
        {"color": b.get("command_color")} for b in results
    ]
    if command_kwargs is None:
        command_kwargs = benchmark_kwargs
    for b in results:
        if args.baseline is not None and b.get("command_name", b["command"]) == args.baseline:
            timing_baseline = b["mean"]
    data.append([b["mean"] for b in results])
    if args.benchmark_names:
        inputs.append(args.benchmark_names[i])
    else:
        inputs.append(filename.stem)

# determine if this is a relative benchmark
relative_timing = timing_baseline is not None
make_relative_timing = lambda timing: (timing / timing_baseline) * 100.0

data = np.transpose([
    [
        round(
            make_relative_timing(timing) if relative_timing else timing,
            2
        )
        for timing in timings
    ]
    for timings in data
])
width = 0.25 # the width of the bars
x = (np.arange(len(data)) + 1) * width

fig, ax = plt.subplots(layout="constrained")
fig.set_figheight(5)
fig.set_figwidth(10)
for i, (offset, command) in enumerate(zip(x, commands)):
    rects = ax.bar(offset, data[i], width, label=command, **command_kwargs[i])

# hide the xticks
ax.tick_params(
    which='both',
    bottom=False,
    top=False,
    labelbottom=False,
)

# show values as on top of bars
display_range = max(*(timing for [timing] in data))
for i, [timing] in zip(x, data):
    if timing > 0:
        if timing_baseline is None:
            label = str(timing)
        else:
            label = mtick.PercentFormatter().format_pct(timing, display_range)
        ax.text(i, timing, label, horizontalalignment='center')

ax.grid(visible=True, axis="y")

if args.title:
    plt.title(args.title)

plt.xlabel("Benchmarks")
plt.ylabel("Time [s]" if timing_baseline is None else "Relative Time [%]")

if timing_baseline is not None:
    ax.yaxis.set_major_formatter(mtick.PercentFormatter())

plt.legend()

# grey-out the text for any legend entry that contains "not included"
for text in plt.legend().get_texts():
    is_included = str(text).find("not included") < 0
    if not is_included:
        text.set_color('grey')

if args.output:
    plt.savefig(args.output)
else:
    plt.show()
