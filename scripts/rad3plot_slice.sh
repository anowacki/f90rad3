#!/bin/bash
# Produce a timeslice of a set of GPR surveys

# Defaults
width_default=15 # / cm

usage () {
	{
		cat <<-END
		Usage: $(basename $0) (options) [twtt]
		Plots a time slice of a grid using rad3slice
		Arguments:
		   twtt         : The two-way travel time to plot
		Options:
		   -a [amp]     : Set max amplitude of trace [auto]
		   -b           : Batch mode [display plot]
		   -o [outfile] : Save plot to file [temp file]
		   -s [scale]   : Set scale [not set]
		   -t [title]   : Add title to plot [none]
		   -w [width]   : Plot width / cm [15]
		   -h(help)     : Print this message
		END
	} >&2
	exit 1
}

# Process arguments
[ $# -eq 0 ] && usage
while [ -n "$1" ]; do
	case "$1" in
		-a) Amax="$2"; shift 2 ;;
		-b) batch=1; shift ;;
		-h|-help) usage 2>&1 ;;
		-o) outfile="$2"; shift 2 ;;
		-s) [ -z "$width" ] && scale="$2" || usage; shift 2 ;;
		-t) title="$2"; shift 2 ;;
		-w) [ -z "$scale" ] && width="$2" || usage; shift 2 ;;
		*) [ $# -ne 1 ] && usage; twtt="$1"; shift ;;
	esac
done

# Check the input file is present
[ ! -r rad3slice.in ] &&
	echo "$(basename $0): Error: file 'rad3slice.in' must be present" > /dev/stderr &&
	exit 1

[ -z "$width" -a -z "$scale" ] && width="${width_default}"

GRD=$(mktemp /tmp/plot_slice.grdXXXXXX)
FIG=$(mktemp /tmp/plot_slice.psXXXXXX)
CPT=$(mktemp /tmp/plot_slice.cptXXXXXX)

trap "rm -f $GRD $FIG $CPT" EXIT

# Get plot information
set -- $(rad3slice $twtt 2>&1 >/dev/null | awk '
	$1=="Limits" {x1=$5; x2=$6; y1=$7; y2=$8}
	$1=="Grid"   {dx=$4; dy=$5}
	$1=="Maximum"{Amax=$3}
	END {print x1-dx, x2+dx, y1-dy, y2+dy, dx, dy, Amax}')
x1=$1; x2=$2; y1=$3; y2=$4; dx=$5; dy=$6
R=$x1/$x2/$y1/$y2
[ -z "$Amax" ] && Amax=$7
dA=$(awk -v a="$Amax" 'BEGIN{print a/40}')

# Make plot
rad3slice $twtt 2>/dev/null | xyz2grd -R$R -I$dx/$dy -G$GRD
makecpt -Cpolar -T-$Amax/$Amax/$dA -D > "$CPT"
[ -n "$scale" ] && J="x$scale" || J=X${width}c/0
grdimage "$GRD" -J$J -R -C"$CPT" -P -Ba5:x:/a5:y::."$title":nSeW> "$FIG"
[ -n "$outfile" ] && cp "$FIG" "$outfile"
[ -z "$batch" ] && gv "$FIG" 2>/dev/null
