(:default
 (:audio-device . "alsa")
 (:audio-pasuspend . nil)
 (:audio-input . "plughw:2,1")
 (:audio-record-sample-rate . 96000)

 (:video-input ":0.0+3840,0"))


(:pulse
 (:audio-device . "pulse")
 (:audio-pasuspend . nil)
 (:audio-input . "default")

 (:video-input ":0.0+3840,0"))
