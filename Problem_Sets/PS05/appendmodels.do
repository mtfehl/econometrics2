sysuse auto, clear
eststo clear
*FIRST SET OF 5 MODELS
local j 1
local model
rename weight weight_1
foreach var in mpg turn length gear disp{
    regress `var' weight if !foreign
    mat obs= e(b)
        forval i=1/`=colsof(obs)'{
        mat obs[1, `i']=e(N)
    }
    estadd matrix obs= obs
    eststo m`j'
    local model "`model' `" Model `j' "'"
    local ++j
}

*SECOND SET OF 5 MODELS
rename weight weight_2
local j 1
foreach var in mpg turn length gear disp{
    regress `var' weight if foreign
    mat obs= e(b)
        forval i=1/`=colsof(obs)'{
        mat obs[1, `i']=e(N)
    }
    estadd matrix obs= obs
    eststo k`j'
    local ++j
}

*PROGRAM TO APPEND MODELS
capt prog drop appendmodels
*! version 1.0.0  14aug2007  Ben Jann
program appendmodels, eclass
    // using first equation of model
    version 8
    syntax namelist
    tempname b V tmp
    foreach name of local namelist {
        qui est restore `name'
        mat `tmp' = e(b)
        local eq1: coleq `tmp'
        gettoken eq1 : eq1
        mat `tmp' = `tmp'[1,"`eq1':"]
        local cons = colnumb(`tmp',"_cons")
        if `cons'<. & `cons'>1 {
            mat `tmp' = `tmp'[1,1..`cons'-1]
        }
        mat `b' = nullmat(`b') , `tmp'
        mat `tmp' = e(V)
        mat `tmp' = `tmp'["`eq1':","`eq1':"]
        if `cons'<. & `cons'>1 {
            mat `tmp' = `tmp'[1..`cons'-1,1..`cons'-1]
        }
        capt confirm matrix `V'
        if _rc {
            mat `V' = `tmp'
        }
        else {
            mat `V' = ///
            ( `V' , J(rowsof(`V'),colsof(`tmp'),0) ) \ ///
            ( J(rowsof(`tmp'),colsof(`V'),0) , `tmp' )
        }
    }
    local names: colfullnames `b'
    mat coln `V' = `names'
    mat rown `V' = `names'
    eret post `b' `V'
    eret local cmd "whatever"
end

*APPEND THE MODELS
forval i=1/`=`j'-1'{
    eststo M`i': appendmodels m`i' k`i'
}

esttab M*,  cells(b(star fmt(3)) se(par fmt(3)) obs(par([ ]) fmt(a)))  mlab(`model')  ///
collabels(none) coeflab(weight_1 "Domestic"  weight_2 "Foreign") nonumbers noobs