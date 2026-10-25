part def Camera {
	private import PictureTaking::*;

	perform action takePicture[*] :> PictureTaking::takePicture;

	part focusingSubsystem {
		perform takePicture::focus;
	}

	part imagingSubsystem {
		perform takePicture::shoot;
	}

}
