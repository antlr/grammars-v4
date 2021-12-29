detector1 = find-only METS_CONFIGURATION
detector2 = find-one METS_DETECTOR where Number = 1
detector2 = find-only METS_DETECTOR where Number = 1
detector3 = find-one METS_DETECTOR where Number less-than 0
detector4 = find-one METS_DETECTOR where Number = 1 &  Type = "Fog"
detector5 = find-one METS_DETECTOR where (Number = 1) | (Number = 2)
detector6 = find-one {all_detectors} where Number = 1
{all_detectors} = find-all METS_DETECTOR
{fog_detectors} = find METS_DETECTOR where Type = "Fog"
{fog_detectors} = find METS_DETECTOR where Type = "Fog" ordered by ID
{fog_detectors} = find METS_DETECTOR where Type = "Fog" reverse ordered by Time
{fog_detectors} = find {all_detectors} where Type = "Fog"
