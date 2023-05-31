#include <TMB.hpp>
/***************************************************/
/* Function to inverse logit transform for vectors */
/***************************************************/
template <class vector>
vector invlogit_vec(vector x){
	vector y = 1.0 / (1.0 + exp(-x));
	return y;
}
/*******************************************************************/
/* Function to inverse logit transform on a general interval [a,b] */
/*******************************************************************/
template <class Type>
Type geninvlogit(Type x, Type a, Type b){
	Type y; 
	y = 1.0 / (1.0 + exp(-x));
	y = y * (b - a) + a;
	return y;
}
/************************************************************************/
/* Objective function to specify model and to optimize model parameters */
/************************************************************************/
template<class Type>
Type objective_function<Type>::operator() ()
{
	using namespace density;
	
    ////////////////////////
    /// Data definitions ///
    ////////////////////////
	// Survival analysis matrices
    DATA_SPARSE_MATRIX(A_mmc); // Matrix selecting instantaneous hazard for medically circumcised pop
    DATA_SPARSE_MATRIX(A_tmc); // Matrix selecting instantaneous hazard for traditionally circumcised pop
    DATA_SPARSE_MATRIX(B); // Matrix selecting relevant cumulative hazard entry for observed and right censored pop
    DATA_SPARSE_MATRIX(C); // Matrix selecting relevant cumulative hazard entry for interval censored pop
    DATA_SPARSE_MATRIX(IntMat1); // Integration matrix for cumulative hazard 
    DATA_SPARSE_MATRIX(IntMat2); // Integration matrix for lagged cumulative hazard 
	
	// Design matrices 
    DATA_SPARSE_MATRIX(X_fixed_tmc); // Design matrix for the fixed effects in the traditional circumcision probabilities
    DATA_SPARSE_MATRIX(X_age_tmc); // Design matrix for the age random effects in the traditional circumcision probabilities
    DATA_SPARSE_MATRIX(X_space_tmc); // Design matrix for the space random effects in the traditional circumcision probabilities
    DATA_SPARSE_MATRIX(X_agespace_tmc); // Design matrix for the age-spatial random effects in the traditional circumcision probabilities
    DATA_SPARSE_MATRIX(X_fixed_mmc_y); // Design matrix for the fixed effects in the youth medical circumcision probabilities
    DATA_SPARSE_MATRIX(X_age_mmc_y); // Design matrix for the age random effects in the youth medical circumcision probabilities
    DATA_SPARSE_MATRIX(X_space_mmc_y); // Design matrix for the spatial random effects in the youth medical circumcision probabilities
    DATA_SPARSE_MATRIX(X_agespace_mmc_y); // Design matrix for the age-space random effects in the youth medical circumcision probabilities
    DATA_SPARSE_MATRIX(X_fixed_mmc_a); // Design matrix for the fixed effects in the adult medical circumcision probabilities
    DATA_SPARSE_MATRIX(X_time_mmc_a); // Design matrix for the temporal random effects in the adult medical circumcision probabilities
    DATA_SPARSE_MATRIX(X_age_mmc_a); // Design matrix for the age random effects in the adult medical circumcision probabilities
    DATA_SPARSE_MATRIX(X_space_mmc_a); // Design matrix for the spatial random effects in the adult medical circumcision probabilities
    DATA_SPARSE_MATRIX(X_agetime_mmc_a); // Design matrix for the age-time random effects in the adult medical circumcision probabilities
    DATA_SPARSE_MATRIX(X_agespace_mmc_a); // Design matrix for the age-space random effects in the adult medical circumcision probabilities
    DATA_SPARSE_MATRIX(X_spacetime_mmc_a); // Design matrix for the space-time random effects in the adult medical circumcision probabilities
	
	// Precision matrices 
    DATA_SPARSE_MATRIX(Q_space); // Aggregation matrix for number of circumcisions performed
	
	// Program data
    DATA_SPARSE_MATRIX(Agg); // Aggregation matrix to aggregate the number of circumcisions performed over age groups
	DATA_VECTOR(NCirc); // Number of circumcisions performed 
	
	// TMC crossover proportions 
    DATA_SPARSE_MATRIX(X_prob); // Design matrix for the TMI crossover probabilities
	DATA_VECTOR(probs_pmn); // Priors for the mean of the probabilities
	DATA_VECTOR(probs_psd); // Priors for the StdDev of the probabilities
		
    //////////////////
    /// Parameters ///
    //////////////////
	// Fixed Effects
    PARAMETER_VECTOR(u_fixed_tmc);
    PARAMETER_VECTOR(u_fixed_mmc_y);
    PARAMETER_VECTOR(u_fixed_mmc_a);
	
	// Age random effect
    PARAMETER_VECTOR(u_age_tmc); 
    PARAMETER_VECTOR(u_age_mmc_y); 
    PARAMETER_VECTOR(u_age_mmc_a); 
	
	// Temporal random effects 
    PARAMETER_VECTOR(u_time_mmc_a);
	
	// Spatial random effect
    PARAMETER_VECTOR(u_space_tmc); 
    PARAMETER_VECTOR(u_space_mmc_y); 
    PARAMETER_VECTOR(u_space_mmc_a); 
	
	// Interactions
	PARAMETER_ARRAY(u_agespace_tmc);
	PARAMETER_ARRAY(u_agespace_mmc_y);
	PARAMETER_ARRAY(u_agespace_mmc_a);
	PARAMETER_ARRAY(u_agetime_mmc_a);
	PARAMETER_ARRAY(u_spacetime_mmc_a);
	
	// Random effects for the proportion of TMI circumcision in VMMC programs
    PARAMETER_VECTOR(u_probs);
	
	// Standard deviations 
    PARAMETER(logsigma_age_tmc);         Type sigma_age_tmc         = exp(logsigma_age_tmc);
    PARAMETER(logsigma_space_tmc);       Type sigma_space_tmc       = exp(logsigma_space_tmc);
    PARAMETER(logsigma_agespace_tmc);    Type sigma_agespace_tmc    = exp(logsigma_agespace_tmc);
    PARAMETER(logsigma_age_mmc_y);       Type sigma_age_mmc_y       = exp(logsigma_age_mmc_y);
    PARAMETER(logsigma_space_mmc_y);     Type sigma_space_mmc_y     = exp(logsigma_space_mmc_y);
    PARAMETER(logsigma_agespace_mmc_y);  Type sigma_agespace_mmc_y  = exp(logsigma_agespace_mmc_y);
    PARAMETER(logsigma_age_mmc_a);       Type sigma_age_mmc_a       = exp(logsigma_age_mmc_a);
    PARAMETER(logsigma_time_mmc_a);      Type sigma_time_mmc_a      = exp(logsigma_time_mmc_a);
    PARAMETER(logsigma_space_mmc_a);     Type sigma_space_mmc_a     = exp(logsigma_space_mmc_a);
    PARAMETER(logsigma_agetime_mmc_a);   Type sigma_agetime_mmc_a   = exp(logsigma_agetime_mmc_a);
    PARAMETER(logsigma_agespace_mmc_a);  Type sigma_agespace_mmc_a  = exp(logsigma_agespace_mmc_a);
    PARAMETER(logsigma_spacetime_mmc_a); Type sigma_spacetime_mmc_a = exp(logsigma_spacetime_mmc_a);

	// Autocorrelation parameters 
    PARAMETER(logitrho_tmc_age1);     Type rho_tmc_age1     = geninvlogit(logitrho_tmc_age1,  Type(-1.0), Type(1.0));
    PARAMETER(logitrho_tmc_age2);     Type rho_tmc_age2     = geninvlogit(logitrho_tmc_age2,  Type(-1.0), Type(1.0));
    PARAMETER(logitrho_mmc_y_age1);   Type rho_mmc_y_age1   = geninvlogit(logitrho_mmc_y_age1,  Type(-1.0), Type(1.0));
    PARAMETER(logitrho_mmc_y_age2);   Type rho_mmc_y_age2   = geninvlogit(logitrho_mmc_y_age2,  Type(-1.0), Type(1.0));
    PARAMETER(logitrho_mmc_a_time1);  Type rho_mmc_a_time1  = geninvlogit(logitrho_mmc_a_time1, Type(-1.0), Type(1.0));
    PARAMETER(logitrho_mmc_a_time2);  Type rho_mmc_a_time2  = geninvlogit(logitrho_mmc_a_time2, Type(-1.0), Type(1.0));
    PARAMETER(logitrho_mmc_a_time3);  Type rho_mmc_a_time3  = geninvlogit(logitrho_mmc_a_time3, Type(-1.0), Type(1.0));
    PARAMETER(logitrho_mmc_a_age1);   Type rho_mmc_a_age1   = geninvlogit(logitrho_mmc_a_age1,  Type(-1.0), Type(1.0));
    PARAMETER(logitrho_mmc_a_age2);   Type rho_mmc_a_age2   = geninvlogit(logitrho_mmc_a_age2,  Type(-1.0), Type(1.0));
    PARAMETER(logitrho_mmc_a_age3);   Type rho_mmc_a_age3   = geninvlogit(logitrho_mmc_a_age3,  Type(-1.0), Type(1.0));
	
	//////////////////////////////////
	/// Prior on the fixed effects ///
	//////////////////////////////////
    // Negative log likelihood definition
    Type nll = Type(0);

	// Fixed effects for the traditional circumcision rate
    nll -= dnorm(u_fixed_tmc, Type(0), Type(5), TRUE).sum();
	
	// Fixed effects for the medical circumcision rate
    nll -= dnorm(u_fixed_mmc_y, Type(0), Type(5), TRUE).sum();
	
	// Fixed effects for the medical circumcision rate
    nll -= dnorm(u_fixed_mmc_a, Type(0), Type(5), TRUE).sum();
	
	////////////////////////////////////////////
	/// Prior on the temporal random effects ///
	////////////////////////////////////////////
	// AR1 Process
	nll += AR1(rho_mmc_a_time1)(u_time_mmc_a);

	// Sum to zero constraint
	nll -= dnorm(u_time_mmc_a.sum(), Type(0), Type(0.001) * u_time_mmc_a.size(), TRUE);

	// Prior on the standard deviation for the temporal random effects
	nll -= dexp(sigma_time_mmc_a, Type(1), TRUE) + logsigma_time_mmc_a;

	// Prior on the logit autocorrelation parameters
	nll -= dnorm(logitrho_mmc_a_time1, Type(3), Type(2), TRUE);
	
	///////////////////////////////////////
	/// Prior on the age random effects ///
	///////////////////////////////////////
	// AR1 processes
	nll += AR1(rho_tmc_age1)(u_age_tmc);
	nll += AR1(rho_mmc_y_age1)(u_age_mmc_y);
	nll += AR1(rho_mmc_a_age1)(u_age_mmc_a);

	// Sum to zero constraints
	nll -= dnorm(u_age_tmc.sum(), Type(0), Type(0.001) * u_age_tmc.size(), TRUE);
	nll -= dnorm(u_age_mmc_y.sum(), Type(0), Type(0.001) * u_age_mmc_y.size(), TRUE);
	nll -= dnorm(u_age_mmc_a.sum(), Type(0), Type(0.001) * u_age_mmc_a.size(), TRUE);
	
	// Prior on the standard deviation for the age random effects
	nll -= dexp(sigma_age_tmc, Type(1), TRUE) + logsigma_age_tmc;
	nll -= dexp(sigma_age_mmc_y, Type(1), TRUE) + logsigma_age_mmc_y;
	nll -= dexp(sigma_age_mmc_a, Type(1), TRUE) + logsigma_age_mmc_a;

	// Prior on the logit autocorrelation parameters
	nll -= dnorm(logitrho_tmc_age1, Type(3), Type(2), TRUE);
	nll -= dnorm(logitrho_mmc_y_age1, Type(3), Type(2), TRUE);
	nll -= dnorm(logitrho_mmc_a_age1, Type(3), Type(2), TRUE);

	///////////////////////////////////////////
	/// Prior on the spatial random effects ///
	///////////////////////////////////////////
	// Gaussian markov random field with prespecified precision matrix
	nll += GMRF(Q_space)(u_space_tmc);
	nll += GMRF(Q_space)(u_space_mmc_y);
	nll += GMRF(Q_space)(u_space_mmc_a);

	// Sum to zero constraints
	nll -= dnorm(u_space_tmc.sum(), Type(0), Type(0.001) * u_space_tmc.size(), TRUE);
	nll -= dnorm(u_space_mmc_y.sum(), Type(0), Type(0.001) * u_space_mmc_y.size(), TRUE);
	nll -= dnorm(u_space_mmc_a.sum(), Type(0), Type(0.001) * u_space_mmc_a.size(), TRUE);

	    // Prior on the standard deviation for the spatial random effects
	nll -= dexp(sigma_space_tmc, Type(1), TRUE) + logsigma_space_tmc;
	nll -= dexp(sigma_space_mmc_y, Type(1), TRUE) + logsigma_space_mmc_y;
	nll -= dexp(sigma_space_mmc_a, Type(1), TRUE) + logsigma_space_mmc_a;

	///////////////////////////////////////////////
	/// Prior on the interaction random effects ///
	///////////////////////////////////////////////
	// Interactions: space-time (GMRF x AR1), age-time (AR1 x AR1) and age-space (AR1 x GMRF)
	nll += SEPARABLE(GMRF(Q_space), AR1(rho_tmc_age2))(u_agespace_tmc);
	nll += SEPARABLE(GMRF(Q_space), AR1(rho_mmc_y_age2))(u_agespace_mmc_y);
	nll += SEPARABLE(GMRF(Q_space), AR1(rho_mmc_a_age3))(u_agespace_mmc_a);
	nll += SEPARABLE(AR1(rho_mmc_a_time2), AR1(rho_mmc_a_age2))(u_agetime_mmc_a);
	nll += SEPARABLE(GMRF(Q_space), AR1(rho_mmc_a_time3))(u_spacetime_mmc_a);

	// Sum-to-zero constraints
	for (int i = 0; i < u_agespace_tmc.cols(); i++) {
		nll -= dnorm(u_agespace_tmc.col(i).sum(), Type(0), Type(0.001) * u_agespace_tmc.col(i).size(), true);
	}
	for (int i = 0; i < u_agespace_mmc_y.cols(); i++) {
		nll -= dnorm(u_agespace_mmc_y.col(i).sum(), Type(0), Type(0.001) * u_agespace_mmc_y.col(i).size(), true);
	}
	for (int i = 0; i < u_agespace_mmc_a.cols(); i++) {
		nll -= dnorm(u_agespace_mmc_a.col(i).sum(), Type(0), Type(0.001) * u_agespace_mmc_a.col(i).size(), true);
	}
	for (int i = 0; i < u_agetime_mmc_a.cols(); i++) {
		nll -= dnorm(u_agetime_mmc_a.col(i).sum(), Type(0), Type(0.001) * u_agetime_mmc_a.col(i).size(), true);
	}
	for (int i = 0; i < u_spacetime_mmc_a.cols(); i++) {
		nll -= dnorm(u_spacetime_mmc_a.col(i).sum(), Type(0), Type(0.001) * u_spacetime_mmc_a.col(i).size(), true);
	}

	// Vectorising the interaction
	vector<Type> u_agespace_tmc_v(u_agespace_tmc);
	vector<Type> u_agespace_mmc_y_v(u_agespace_mmc_y);
	vector<Type> u_agespace_mmc_a_v(u_agespace_mmc_a);
	vector<Type> u_agetime_mmc_a_v(u_agetime_mmc_a);
	vector<Type> u_spacetime_mmc_a_v(u_spacetime_mmc_a);

	// Prior on the standard deviation for the interaction random effects
	nll -= dexp(sigma_agespace_tmc,  Type(1), TRUE) + logsigma_agespace_tmc;
	nll -= dexp(sigma_agespace_mmc_y,  Type(1), TRUE) + logsigma_agespace_mmc_y;
	nll -= dexp(sigma_agespace_mmc_a,  Type(1), TRUE) + logsigma_agespace_mmc_a;
	nll -= dexp(sigma_agetime_mmc_a,   Type(1), TRUE) + logsigma_agetime_mmc_a;
	nll -= dexp(sigma_spacetime_mmc_a, Type(1), TRUE) + logsigma_spacetime_mmc_a;

	// Prior on the logit autocorrelation parameters
	nll -= dnorm(logitrho_tmc_age2,  Type(3), Type(2), TRUE);
	nll -= dnorm(logitrho_mmc_a_age2,  Type(3), Type(2), TRUE);
	nll -= dnorm(logitrho_mmc_a_time2, Type(3), Type(2), TRUE);
	nll -= dnorm(logitrho_mmc_a_age2,  Type(3), Type(2), TRUE);
	nll -= dnorm(logitrho_mmc_a_time3, Type(3), Type(2), TRUE);
	nll -= dnorm(logitrho_mmc_a_age3,  Type(3), Type(2), TRUE);
	
    ///////////////////////////////////
    /// Proportion of TMI borrowing ///
    ///////////////////////////////////
	// Log precision for the stratification random effects
	nll -= dnorm(u_probs, probs_pmn, probs_psd, TRUE).sum();
	
	// Vector of probabilities for the 
    vector<Type> probs = X_prob * invlogit(u_probs);
	
    //////////////////////////////
    /// Estimating hazard rate ///
    //////////////////////////////
	// Traditional hazard rate
	vector<Type> haz_tmc = X_fixed_tmc * u_fixed_tmc +
		X_space_tmc * u_space_tmc * sigma_space_tmc +
			X_age_tmc * u_age_tmc * sigma_age_tmc +
					X_agespace_tmc * u_agespace_tmc_v * sigma_agespace_tmc;
	
	// Medical hazard rate
	vector<Type> haz_mmc = X_fixed_mmc_y * u_fixed_mmc_y +
		X_space_mmc_y * u_space_mmc_y * sigma_space_mmc_y +
			X_age_mmc_y * u_age_mmc_y * sigma_age_mmc_y +
					X_agespace_mmc_y * u_agespace_mmc_y_v * sigma_agespace_mmc_y +
						X_fixed_mmc_a * u_fixed_mmc_a +
							X_time_mmc_a * u_time_mmc_a * sigma_time_mmc_a +
								X_space_mmc_a * u_space_mmc_a * sigma_space_mmc_a +
									X_age_mmc_a * u_age_mmc_a * sigma_age_mmc_a +
										X_agetime_mmc_a * u_agetime_mmc_a_v * sigma_agetime_mmc_a +
											X_agespace_mmc_a * u_agespace_mmc_a_v * sigma_agespace_mmc_a +
												X_spacetime_mmc_a * u_spacetime_mmc_a_v * sigma_spacetime_mmc_a;
	
	// Rates on [0,1] scale 
    haz_tmc = invlogit_vec(haz_tmc);
    haz_mmc = invlogit_vec(haz_mmc);

	// Adjustment such that \lambda_mmc + \lambda_tmc \in [0,1]  
	// Medical rate to only take from the remaining proportion 
	// not taken through traditional circumcision (1 - \lambda_tmc)
    haz_mmc = haz_mmc * (1 - haz_tmc);
	
	// Total hazard rate
	vector<Type> haz = haz_mmc + haz_tmc;
	
	// Survival probabilities
	vector<Type> logprob  = log(Type(1.0) - haz);
	vector<Type> surv     = exp(IntMat1 * logprob);
	vector<Type> surv_lag = exp(IntMat2 * logprob);
	vector<Type> leftcens = Type(1.0) - surv;

	// Incidence 
	vector<Type> inc_tmc  = (Type(1.0) - probs) * haz_tmc * surv_lag;
	vector<Type> inc_mmc  = haz_mmc * surv_lag;
	vector<Type> inc_mmct = probs * haz_tmc * surv_lag;
	
	// Cumulative incidence 
	vector<Type> cum_inc_tmc  = IntMat1 * inc_tmc;
	vector<Type> cum_inc_mmc  = IntMat1 * inc_mmc;
	vector<Type> cum_inc_mmct = IntMat1 * inc_mmct;
	
	// Estimated number of circumcisions performed 
    vector<Type> rate = inc_mmc + inc_mmct;
    vector<Type> mu_circ = Agg * rate;

    //////////////////
    /// Likelihood ///
    //////////////////
	// Getting likelihood for those medically circumcised
	nll -= (A_mmc * log(haz_mmc) + A_mmc * log(surv_lag)).sum();

	// Getting likelihood for those traditionally circumcised
	nll -= (A_tmc * log(haz_tmc) + A_tmc * log(surv_lag)).sum();

	// Getting likelihood for those right censored
	nll -= (B * log(surv)).sum();

	// Getting likelihood for those left censored
	nll -= (C * log(leftcens)).sum();

	// Getting likelihood for the program data (Poisson likelihood)
	nll -= dpois(NCirc, mu_circ, TRUE).sum();

    ///////////////////////////
    /// Reporting variables ///
    ///////////////////////////
    REPORT(haz_mmc);      // Medical hazard rate
    REPORT(haz_tmc);      // Traditional hazard rate
    REPORT(inc_tmc);      // Traditional circumcision incidence rate
    REPORT(inc_mmc);      // Medical circumcision incidence rate
    REPORT(inc_mmct);     // Medical circumcision in traditional settings  incidence rate
    REPORT(cum_inc_tmc);  // Traditional circumcision cumulative incidence rate
    REPORT(cum_inc_mmc);  // Medical circumcision cumulative incidence rate
    REPORT(cum_inc_mmct); // Medical circumcision in traditional context cumulative incidence rate
    REPORT(surv);         // Survival probabilities
	REPORT(mu_circ);      // Mean of the number of circumcisions performed 
	REPORT(probs);        // Proportions of TMI in MMCs 
	
    /////////////////////////////////////////
    /// Returning negative log likelihood ///
    /////////////////////////////////////////
    return nll;
}
