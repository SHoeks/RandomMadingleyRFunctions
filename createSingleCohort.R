createSingleCohort = function(cohortAdultMass_kg, gc_index, functional_group, isAdult = 0, sp_inputs, cohort_def=0) {


  # example:
  # library(MadingleyR)
  # sp_inputs = madingley_inputs("spatial inputs")
  # createSingleCohort(cohortAdultMass_kg=200,gc_index=0,functional_group=0,isAdult=1,sp_inputs=sp_inputs)
  
  if(cohort_def==0) cohort_def = MadingleyR::madingley_inputs('cohort definition')
  
  
  functional_group_index=functional_group+1
    
  TrophicIndex = 0
  if(cohort_def$DEFINITION_Nutrition.source[functional_group_index] == "Herbivore") TrophicIndex = 2.0
  if(cohort_def$DEFINITION_Nutrition.source[functional_group_index] == "Carnivore") TrophicIndex = 3.0
  if(cohort_def$DEFINITION_Nutrition.source[functional_group_index] == "Omnivore")  TrophicIndex = 2.5
   
    
  optimalPreyBodySizeRatio = 0
  if( cohort_def$DEFINITION_Endo.Ectotherm[functional_group_index] == "Endotherm" && cohortAdultMass_kg >= 21 && 
      cohort_def$DEFINITION_Nutrition.source[functional_group_index] == "Carnivore" ) {
    
    optimalPreyBodySizeRatio = max( 0.01, rnorm( 1, 1, 0.02 ) );
    
  }else{
    
    optimalPreyBodySizeRatio = max( 0.01, rnorm( 1, 0.1, 0.02 ) );
    
  }   
  #print(optimalPreyBodySizeRatio)
  
  options(warn=-1)
  cohortAdultMass = cohortAdultMass_kg*1000
  expectedLnAdultMassRatio = 2.24 + 0.13 * log( cohortAdultMass );
  cohortAdultMassRatio = 1.0 + 10 * rnorm(n=1, mean=expectedLnAdultMassRatio, sd=0.5)
  cohortJuvenileMass = cohortAdultMass * 1.0 / cohortAdultMassRatio;
  NewBiomass = ( 3300. / 1000 ) * 100 * 3000 * 0.6^log10( cohortJuvenileMass ) * ( mean(area(sp_inputs$Endo_H_max)[]/10000) )
  options(warn=0)
  
  out = data.frame(
    GridcellIndex = gc_index,
    FunctionalGroupIndex = functional_group,
    JuvenileMass = cohortJuvenileMass,
    AdultMass = cohortAdultMass,
    IndividualBodyMass = cohortJuvenileMass,
    CohortAbundance = NewBiomass,
    LogOptimalPreyBodySizeRatio = log(optimalPreyBodySizeRatio),
    BirthTimeStep = 0,
    ProportionTimeActive = 0.5,
    TrophicIndex = TrophicIndex,
    IndividualReproductivePotentialMass = 0,
    MaturityTimeStep = 0,
    IsAdult = 0,
    AgeMonths = 0,                      
    TimeStepsJuviline = 0,            
    TimeStepsAdult = 0  
  )
  
  if(isAdult==1) {
    out$IndividualBodyMass = cohortAdultMass
    out$IsAdult = 1
    out$TimeStepsAdult = 1
    out$AgeMonths = 1
    out$TimeStepsJuviline = 1
  }
  
  return(out)

}
