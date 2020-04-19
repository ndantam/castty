src/%.mkv: rec/%.ogv
	mkdir -p src
	ffmpeg -i "$<" \
	 -codec:v libx264 -crf 0 \
	-codec:a copy \
	-max_muxing_queue_size 4096 \
	"$@"

# Notes:
# - lossless libx264 was smaller than libvpx-vp9
#   -codec:v libvpx-vp9 -lossless 1 \
