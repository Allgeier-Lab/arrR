// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_arrR_RCPPEXPORTS_H_GEN_
#define RCPP_arrR_RCPPEXPORTS_H_GEN_

#include <Rcpp.h>

namespace arrR {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("arrR", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("arrR", "_arrR_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in arrR");
            }
        }
    }

    inline void rcpp_diffuse_values(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix cell_adj, double nutrients_diffusion, double detritus_diffusion, double detritus_fish_diffusion) {
        typedef SEXP(*Ptr_rcpp_diffuse_values)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_rcpp_diffuse_values p_rcpp_diffuse_values = NULL;
        if (p_rcpp_diffuse_values == NULL) {
            validateSignature("void(*rcpp_diffuse_values)(Rcpp::NumericMatrix,Rcpp::NumericMatrix,double,double,double)");
            p_rcpp_diffuse_values = (Ptr_rcpp_diffuse_values)R_GetCCallable("arrR", "_arrR_rcpp_diffuse_values");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_rcpp_diffuse_values(Shield<SEXP>(Rcpp::wrap(seafloor)), Shield<SEXP>(Rcpp::wrap(cell_adj)), Shield<SEXP>(Rcpp::wrap(nutrients_diffusion)), Shield<SEXP>(Rcpp::wrap(detritus_diffusion)), Shield<SEXP>(Rcpp::wrap(detritus_fish_diffusion)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void rcpp_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, Rcpp::NumericMatrix seafloor, double pop_k, double pop_linf, double pop_a, double pop_b, double pop_n_body, double pop_reserves_max, double pop_reserves_consump, Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions, double min_per_i) {
        typedef SEXP(*Ptr_rcpp_fishpop_growth)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_rcpp_fishpop_growth p_rcpp_fishpop_growth = NULL;
        if (p_rcpp_fishpop_growth == NULL) {
            validateSignature("void(*rcpp_fishpop_growth)(Rcpp::NumericMatrix,Rcpp::NumericMatrix,Rcpp::NumericMatrix,double,double,double,double,double,double,double,Rcpp::NumericVector,Rcpp::IntegerVector,double)");
            p_rcpp_fishpop_growth = (Ptr_rcpp_fishpop_growth)R_GetCCallable("arrR", "_arrR_rcpp_fishpop_growth");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_rcpp_fishpop_growth(Shield<SEXP>(Rcpp::wrap(fishpop)), Shield<SEXP>(Rcpp::wrap(fishpop_track)), Shield<SEXP>(Rcpp::wrap(seafloor)), Shield<SEXP>(Rcpp::wrap(pop_k)), Shield<SEXP>(Rcpp::wrap(pop_linf)), Shield<SEXP>(Rcpp::wrap(pop_a)), Shield<SEXP>(Rcpp::wrap(pop_b)), Shield<SEXP>(Rcpp::wrap(pop_n_body)), Shield<SEXP>(Rcpp::wrap(pop_reserves_max)), Shield<SEXP>(Rcpp::wrap(pop_reserves_consump)), Shield<SEXP>(Rcpp::wrap(extent)), Shield<SEXP>(Rcpp::wrap(dimensions)), Shield<SEXP>(Rcpp::wrap(min_per_i)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void rcpp_mineralization(Rcpp::NumericMatrix seafloor, double detritus_mineralization, double detritus_fish_decomp) {
        typedef SEXP(*Ptr_rcpp_mineralization)(SEXP,SEXP,SEXP);
        static Ptr_rcpp_mineralization p_rcpp_mineralization = NULL;
        if (p_rcpp_mineralization == NULL) {
            validateSignature("void(*rcpp_mineralization)(Rcpp::NumericMatrix,double,double)");
            p_rcpp_mineralization = (Ptr_rcpp_mineralization)R_GetCCallable("arrR", "_arrR_rcpp_mineralization");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_rcpp_mineralization(Shield<SEXP>(Rcpp::wrap(seafloor)), Shield<SEXP>(Rcpp::wrap(detritus_mineralization)), Shield<SEXP>(Rcpp::wrap(detritus_fish_decomp)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void rcpp_mortality(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, Rcpp::NumericMatrix seafloor, double pop_linf, double pop_n_body, double pop_reserves_max, Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions) {
        typedef SEXP(*Ptr_rcpp_mortality)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_rcpp_mortality p_rcpp_mortality = NULL;
        if (p_rcpp_mortality == NULL) {
            validateSignature("void(*rcpp_mortality)(Rcpp::NumericMatrix,Rcpp::NumericMatrix,Rcpp::NumericMatrix,double,double,double,Rcpp::NumericVector,Rcpp::IntegerVector)");
            p_rcpp_mortality = (Ptr_rcpp_mortality)R_GetCCallable("arrR", "_arrR_rcpp_mortality");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_rcpp_mortality(Shield<SEXP>(Rcpp::wrap(fishpop)), Shield<SEXP>(Rcpp::wrap(fishpop_track)), Shield<SEXP>(Rcpp::wrap(seafloor)), Shield<SEXP>(Rcpp::wrap(pop_linf)), Shield<SEXP>(Rcpp::wrap(pop_n_body)), Shield<SEXP>(Rcpp::wrap(pop_reserves_max)), Shield<SEXP>(Rcpp::wrap(extent)), Shield<SEXP>(Rcpp::wrap(dimensions)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void rcpp_move_wrap(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef, Rcpp::String movement, Rcpp::NumericVector pop_reserves_thres, double move_mean, double move_var, double move_reef, double move_border, double move_return, double max_dist, Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions) {
        typedef SEXP(*Ptr_rcpp_move_wrap)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_rcpp_move_wrap p_rcpp_move_wrap = NULL;
        if (p_rcpp_move_wrap == NULL) {
            validateSignature("void(*rcpp_move_wrap)(Rcpp::NumericMatrix,Rcpp::NumericMatrix,Rcpp::String,Rcpp::NumericVector,double,double,double,double,double,double,Rcpp::NumericVector,Rcpp::IntegerVector)");
            p_rcpp_move_wrap = (Ptr_rcpp_move_wrap)R_GetCCallable("arrR", "_arrR_rcpp_move_wrap");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_rcpp_move_wrap(Shield<SEXP>(Rcpp::wrap(fishpop)), Shield<SEXP>(Rcpp::wrap(coords_reef)), Shield<SEXP>(Rcpp::wrap(movement)), Shield<SEXP>(Rcpp::wrap(pop_reserves_thres)), Shield<SEXP>(Rcpp::wrap(move_mean)), Shield<SEXP>(Rcpp::wrap(move_var)), Shield<SEXP>(Rcpp::wrap(move_reef)), Shield<SEXP>(Rcpp::wrap(move_border)), Shield<SEXP>(Rcpp::wrap(move_return)), Shield<SEXP>(Rcpp::wrap(max_dist)), Shield<SEXP>(Rcpp::wrap(extent)), Shield<SEXP>(Rcpp::wrap(dimensions)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void rcpp_nutr_input(Rcpp::NumericMatrix seafloor, double nutrients_input) {
        typedef SEXP(*Ptr_rcpp_nutr_input)(SEXP,SEXP);
        static Ptr_rcpp_nutr_input p_rcpp_nutr_input = NULL;
        if (p_rcpp_nutr_input == NULL) {
            validateSignature("void(*rcpp_nutr_input)(Rcpp::NumericMatrix,double)");
            p_rcpp_nutr_input = (Ptr_rcpp_nutr_input)R_GetCCallable("arrR", "_arrR_rcpp_nutr_input");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_rcpp_nutr_input(Shield<SEXP>(Rcpp::wrap(seafloor)), Shield<SEXP>(Rcpp::wrap(nutrients_input)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void rcpp_nutr_output(Rcpp::NumericMatrix seafloor, double nutrients_loss, double detritus_loss) {
        typedef SEXP(*Ptr_rcpp_nutr_output)(SEXP,SEXP,SEXP);
        static Ptr_rcpp_nutr_output p_rcpp_nutr_output = NULL;
        if (p_rcpp_nutr_output == NULL) {
            validateSignature("void(*rcpp_nutr_output)(Rcpp::NumericMatrix,double,double)");
            p_rcpp_nutr_output = (Ptr_rcpp_nutr_output)R_GetCCallable("arrR", "_arrR_rcpp_nutr_output");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_rcpp_nutr_output(Shield<SEXP>(Rcpp::wrap(seafloor)), Shield<SEXP>(Rcpp::wrap(nutrients_loss)), Shield<SEXP>(Rcpp::wrap(detritus_loss)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void rcpp_respiration(Rcpp::NumericMatrix fishpop, double resp_intercept, double resp_slope, double resp_temp_low, double resp_temp_max, double resp_temp_optm, double water_temp, double min_per_i) {
        typedef SEXP(*Ptr_rcpp_respiration)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_rcpp_respiration p_rcpp_respiration = NULL;
        if (p_rcpp_respiration == NULL) {
            validateSignature("void(*rcpp_respiration)(Rcpp::NumericMatrix,double,double,double,double,double,double,double)");
            p_rcpp_respiration = (Ptr_rcpp_respiration)R_GetCCallable("arrR", "_arrR_rcpp_respiration");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_rcpp_respiration(Shield<SEXP>(Rcpp::wrap(fishpop)), Shield<SEXP>(Rcpp::wrap(resp_intercept)), Shield<SEXP>(Rcpp::wrap(resp_slope)), Shield<SEXP>(Rcpp::wrap(resp_temp_low)), Shield<SEXP>(Rcpp::wrap(resp_temp_max)), Shield<SEXP>(Rcpp::wrap(resp_temp_optm)), Shield<SEXP>(Rcpp::wrap(water_temp)), Shield<SEXP>(Rcpp::wrap(min_per_i)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void rcpp_seagrass_growth(Rcpp::NumericMatrix seafloor, Rcpp::NumericVector cells_reef, double bg_v_max, double bg_k_m, double bg_gamma, double ag_v_max, double ag_k_m, double ag_gamma, double bg_biomass_max, double bg_biomass_min, double ag_biomass_max, double ag_biomass_min, double seagrass_thres, double seagrass_slope, double seagrass_slough, double time_frac) {
        typedef SEXP(*Ptr_rcpp_seagrass_growth)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_rcpp_seagrass_growth p_rcpp_seagrass_growth = NULL;
        if (p_rcpp_seagrass_growth == NULL) {
            validateSignature("void(*rcpp_seagrass_growth)(Rcpp::NumericMatrix,Rcpp::NumericVector,double,double,double,double,double,double,double,double,double,double,double,double,double,double)");
            p_rcpp_seagrass_growth = (Ptr_rcpp_seagrass_growth)R_GetCCallable("arrR", "_arrR_rcpp_seagrass_growth");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_rcpp_seagrass_growth(Shield<SEXP>(Rcpp::wrap(seafloor)), Shield<SEXP>(Rcpp::wrap(cells_reef)), Shield<SEXP>(Rcpp::wrap(bg_v_max)), Shield<SEXP>(Rcpp::wrap(bg_k_m)), Shield<SEXP>(Rcpp::wrap(bg_gamma)), Shield<SEXP>(Rcpp::wrap(ag_v_max)), Shield<SEXP>(Rcpp::wrap(ag_k_m)), Shield<SEXP>(Rcpp::wrap(ag_gamma)), Shield<SEXP>(Rcpp::wrap(bg_biomass_max)), Shield<SEXP>(Rcpp::wrap(bg_biomass_min)), Shield<SEXP>(Rcpp::wrap(ag_biomass_max)), Shield<SEXP>(Rcpp::wrap(ag_biomass_min)), Shield<SEXP>(Rcpp::wrap(seagrass_thres)), Shield<SEXP>(Rcpp::wrap(seagrass_slope)), Shield<SEXP>(Rcpp::wrap(seagrass_slough)), Shield<SEXP>(Rcpp::wrap(time_frac)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

}

#endif // RCPP_arrR_RCPPEXPORTS_H_GEN_
