/**
  * @file UnitPathogen.cpp
  * @author P. Casadebaig-(The RECORD team -INRA )
  */

/*
 * Copyright (C) 2010 INRA
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <vle/extension/DifferenceEquation.hpp>
#include <vle/extension/DifferenceEquationDbg.hpp>
#include <vle/devs/DynamicsDbg.hpp>

namespace vd = vle::devs;
namespace ve = vle::extension;
namespace vv = vle::value;

namespace Crop {

class Unit : public ve::DifferenceEquation::Multiple
{
public:
    Unit(
       const vd::DynamicsInit& atom,
       const vd::InitEventList& evts)
        : ve::DifferenceEquation::Multiple(atom, evts)
    {
        // Parametres
        P_AreaMax = vv::toDouble(evts.get("P_AreaMax"));
		P_UnitTTExp = vv::toDouble(evts.get("P_UnitTTExp"));
		P_UnitTTSen = vv::toDouble(evts.get("P_UnitTTSen"));
		P_SlopeExpansion = vv::toDouble(evts.get("P_SlopeExpansion"));
		P_SlopeSenescence = vv::toDouble(evts.get("P_SlopeExpansion"));
        E_RateDeseaseTransmission = vv::toDouble(evts.get("E_RateDeseaseTransmission"));
        E_RateAlloDeposition = vv::toDouble(evts.get("E_RateAlloDeposition"));
        E_InfectiousPeriod = vv::toDouble(evts.get("E_InfectiousPeriod"));
        E_LatentPeriod = vv::toDouble(evts.get("E_LatentPeriod"));
        
        // Variables synchrones
		TempEff = createSync("TempEff"); 
		ThermalTime = createSync("ThermalTime"); 
        ActionTemp = createSync("ActionTemp");
        In = createNosync("in");
        
        // Variables d'état
        RateAreaExpansion = createVar("RateAreaExpansion");
        RateAreaSenescence = createVar("RateAreaSenescence");
		AreaExpansion = createVar("AreaExpansion");
		AreaActive = createVar("AreaActive");
        AreaHealthy = createVar("AreaHealthy");
        AreaLatent = createVar("AreaLatent");
        AreaInfectious = createVar("AreaInfectious");        
        AreaRemoved = createVar("AreaRemoved");
        AreaRemovedByDesease = createVar("AreaRemovedByDesease");
        ScoreArea = createVar("ScoreArea");
        AreaSenescence = createVar("AreaSenescence");
        AreaDeseased = createVar("AreaDeseased");
        RateDeseaseTransmission = createVar("RateDeseaseTransmission");
        RateAlloDeposition = createVar("RateAlloDeposition");
        LatentPeriod = createVar("LatentPeriod");
        InfectiousPeriod = createVar("InfectiousPeriod");
        Out = createVar("out");
        OutDeposition = createVar("OutDeposition");
        InDeposition = createVar("InDeposition");
        Receptivity = createVar("Receptivity");
        ThermalAge = createVar("ThermalAge");
        InitQuantity = createVar("InitQuantity");
        CropState = createVar("CropState");
    }

    virtual ~Unit()
    {}

//@@begin:compute@@
virtual void compute(const vd::Time& /*time*/)
{
    // Quantité d'inoculum primaire (perturbé par modèle Initation)
    InitQuantity = 0;
    
    /* Réception de spores : /!\ dépendante de la veille (t-1) 
     * Géré par 4 variables (pour permettre les perturbations) :
     * In/Out : assurent la connexion entre unités, mappés vers les ports in/out
     * InDeposition/OutDeposition : variables "biologiques", decrit le fonctionnement
     */
    
    double InDeposition_tmp = In(-1);
    if (TempEff() > 20) {
        InDeposition_tmp = 0;
    }
    InDeposition = InDeposition_tmp;  

    // Age thermique de l'unité
    ThermalAge = TempEff() + ThermalAge(-1);
    
    /* Receptivité de tissus : Réponse identique pour toute les unités
     * Linéaire decroissant : Receptivity = fmax(- 1/1000 * ThermalAge() +1, 0);
     * Linéaire croissant : Receptivity = fmin(1/1000 * ThermalAge() + 0.2, 1.2);
     * Sigmoide : Receptivity = 1 / (1 + exp(-0.005 * (ThermalAge() - 500))) + 0.2;
     * (paramètres : pente, asymptotes (haut et bas), abscisse pt d'inflexion)
     */ 
    Receptivity = 1 / (1 + exp(-0.005 * (ThermalAge() - 500))) + 0.2;
    
    /* Periode de latence : durée en j.
     * Pour les parasites biotrophes, cette variable pèse bcp dans le dev de l'épidémie (Rapilly1990, Zadoks1971)
     * Cible de contraintes env & resistance plante. 
     */
    LatentPeriod = E_LatentPeriod; 
     
    /* Periode infectieuse
     */
    InfectiousPeriod = E_InfectiousPeriod;
    
    // Vitesse de croissance
    RateAreaExpansion = TempEff() * (P_AreaMax * P_SlopeExpansion) * exp(-P_SlopeExpansion * (ThermalTime() - P_UnitTTExp)) / pow((1 + exp(-P_SlopeExpansion * (ThermalTime() - P_UnitTTExp))),2);
    
    /* Vitesse de senescence : pose un problème pour déterminer l'asymptote
     * (paramètre) alors qu'il depends de l'évolution de la maladie. 
     * Plutôt que de le résoudre analytiquement, on conditionne cette dérivée.  
     */ 
    double RateAreaSenescence_tmp = TempEff() * (P_AreaMax * P_SlopeSenescence) * exp(-P_SlopeSenescence * (ThermalTime() - P_UnitTTSen)) / pow((1 + exp(-P_SlopeSenescence * (ThermalTime() - P_UnitTTSen))),2);
    if ((AreaHealthy(-1) <= 0) and (AreaRemoved(-1) > P_AreaMax)) {
        RateAreaSenescence_tmp = 0;
    }
    RateAreaSenescence = RateAreaSenescence_tmp;
    
    // Surface totale : intégration de RateAreaExpansion
    AreaExpansion = AreaExpansion(-1) + RateAreaExpansion();
    
    // Surface senescente : intégration de RateAreaSenescence
    AreaSenescence = fmin(AreaSenescence(-1) + RateAreaSenescence(),P_AreaMax);
    
    // Vitesse d'infection au sein de l'unité (autodeposition)
    RateDeseaseTransmission = E_RateDeseaseTransmission;
    
    // Vitesse d'infection entre unités (allodeposition)
    RateAlloDeposition = E_RateAlloDeposition;
        
    // Surface saine, création par la croissance, destruction par l'infection (intra / extra) et la senescence naturelle
    AreaHealthy = fmax(
        AreaHealthy(-1)
        -InitQuantity() 
        +RateAreaExpansion() 
        -(RateDeseaseTransmission() * AreaInfectious(-1) * AreaHealthy(-1)/AreaExpansion(-1)) * Receptivity()
        -(InDeposition() * AreaHealthy(-1)/AreaExpansion(-1)) * Receptivity()
        -(RateAreaSenescence() * AreaHealthy(-1)/AreaActive(-1)),0);
        
    // Surface latente, création par l'infection, destruction par la propagation du pathogene et la senescence
    AreaLatent = fmax(
        AreaLatent(-1)
        +InitQuantity() 
        +(RateDeseaseTransmission() * AreaInfectious(-1) * AreaHealthy(-1)/AreaExpansion(-1)) * Receptivity()
        +(InDeposition()* AreaHealthy(-1)/AreaExpansion(-1)) * Receptivity()
        -(1/LatentPeriod() * AreaLatent(-1)) 
        -(RateAreaSenescence() * AreaLatent(-1)/AreaActive(-1)),0);
        
    // Surface infectieuse, creation par la propagation du pathogene, destruction par le pathogene
    AreaInfectious = fmax(
        AreaInfectious(-1) 
        +(1/LatentPeriod() * AreaLatent(-1)) 
        -(1/InfectiousPeriod() * AreaInfectious(-1)) 
        -(RateAreaSenescence() * AreaInfectious(-1)/AreaActive(-1)),0);
    
    // Surface détruite par le pathogène et la senescence naturelle
    AreaRemoved = fmin( 
        AreaRemoved(-1) 
        +(1/InfectiousPeriod() * AreaInfectious(-1)) 
        +RateAreaSenescence(),P_AreaMax);  
        
    // Surface détruite par la maladie
    AreaRemovedByDesease = 
        AreaRemovedByDesease(-1) 
        +(1/InfectiousPeriod() * AreaInfectious(-1));
        
    // % de Surface détruite par la maladie = note de surface
    ScoreArea = 
        AreaRemovedByDesease() / AreaExpansion();
    
    // Surface active : différence totale - senescence
    AreaActive = fmax(AreaExpansion() - AreaRemoved(),0);
    
    // Surface malade = total des surfaces infectée (L + I + R causé par maladie)
    AreaDeseased =
        + AreaLatent() 
        + AreaInfectious() 
        + (1/InfectiousPeriod() * AreaInfectious(-1));
        
    /* Emission de spores
     * 1. proportion de la surface infectieuse de l'unité
     * 2. réduite par la porosité de l'unité / couvert
     */
    OutDeposition = RateAlloDeposition() * AreaInfectious();
    
    // Emission, pour être coherent avec la nomenclature In/Out
    Out = OutDeposition();
    
    
}
//@@end:compute@@

//@@begin:initValue@@
virtual void initValue(const vd::Time& /*time*/)
{
    RateAreaExpansion = 0.0;
    RateAreaSenescence = 0.0;
    AreaExpansion = 0.001;
    AreaActive = 0.001;
    AreaHealthy = 0.001;
    AreaLatent = 0.0;
    AreaInfectious = 0.0;
    AreaRemoved = 0.0;
    AreaRemovedByDesease = 0.0;
    ScoreArea = 0;
    AreaSenescence = 0.0;
    AreaDeseased = 0.0;
    RateDeseaseTransmission = E_RateDeseaseTransmission;    
    RateAlloDeposition = E_RateAlloDeposition;    
    LatentPeriod = E_LatentPeriod;
    InfectiousPeriod = E_InfectiousPeriod;
    OutDeposition = 0;
    InDeposition = 0;
    Out = 0;
    Receptivity = 0.2;
    ThermalAge = 0;
    InitQuantity = 0;
    CropState = 0;
    
}
//@@end:initValue@@

private:
//@@begin:user@@
//@@end:user@@

    // Parametres
    double P_AreaMax; /**< Paramètre : surface potentielle d'une unité, Unité : m^2 */
    double P_UnitTTExp; /**< Paramètre : date de demi-expansion de la surface d'une unité */
    double P_UnitTTSen; /**< Paramètre : date de demi-senescence de la surface d'une unité */
    double P_SlopeExpansion; /**< Paramètre : acceleration de l'expansion de la surface d'une unité */
    double P_SlopeSenescence ; /**< Paramètre : acceleration de la senescence de la surface d'une unité */  
    double E_RateDeseaseTransmission; /**< Paramètre : Vitesse de transmission de la maladie */
    double E_RateAlloDeposition; /**< Paramètre : Modulation du taux d'allodeposition */
    double E_InfectiousPeriod; /**< Paramètre : durée de la période infectieuse */
    double E_LatentPeriod; /**< Paramètre : durée de la période de latence */
    
    // Entrées
    Sync ActionTemp;
    Sync TempEff;
    Sync ThermalTime;
    Nosync In;
    
    // Variables d'état
    Var RateAreaExpansion;
    Var RateAreaSenescence;
    Var AreaExpansion;
    Var AreaActive;
    Var AreaHealthy;
    Var AreaLatent;
    Var AreaInfectious;
    Var AreaRemoved;
    Var AreaRemovedByDesease;
    Var ScoreArea;
    Var AreaSenescence;
    Var AreaDeseased;
    Var RateDeseaseTransmission;
    Var RateAlloDeposition;
    Var LatentPeriod;
    Var InfectiousPeriod;
    Var OutDeposition;
    Var InDeposition;
    Var Out;
    Var Receptivity;
    Var ThermalAge;
    Var InitQuantity;
    Var CropState;
    
};

} // namespace Unit

//DECLARE_DIFFERENCE_EQUATION_MULTIPLE_DBG(Crop::Unit)
DECLARE_DYNAMICS_DBG(Crop::Unit)
