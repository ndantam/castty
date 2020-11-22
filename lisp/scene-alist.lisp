(:default
 (:audio-device . "alsa")
 (:audio-pasuspend . nil)

 ;;(:audio-input . "hw:VX222e0")


 (:audio-input . "hw:PAD")
 (:audio-record-codec . "pcm_s32le")
 (:audio-record-codec-out . "pcm_s24le")

 ;; (:audio-input . "hw:4,1")
 ;; (:audio-record-sample-rate . 96000)

 (:video-input ":0.0+3840,0"))

(:half
 (:video-size "960x1080"))



(:pulse
 (:audio-device . "pulse")
 (:audio-pasuspend . nil)
 (:audio-input . "default")

 (:video-input ":0.0+3840,0"))
