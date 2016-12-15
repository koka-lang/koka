type fix<f :: V -> V > {
  con Inn( out : f<fix<f>> )
}
