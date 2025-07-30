{DEFAULT @limit = ''}
{DEFAULT @offset = ''}
{DEFAULT @order_by = ''}
{DEFAULT @ascending = 'ASC'}
{DEFAULT @required_benefit_sources = ''}
 -- Should always be the same as number of data sources
{DEFAULT @required_benefit_count = 0}
{DEFAULT @analysis_id = 1}
{DEFAULT @show_exposure_classes = FALSE}
{DEFAULT @outcome_cohort_length = FALSE}
{DEFAULT @target_cohort_length = TRUE}
{DEFAULT @target_cohorts = (1, 2, 3)}
{DEFAULT @filter_by_meta_analysis = FALSE}
{DEFAULT @filter_outcome_types = FALSE}

WITH benefit_t AS(
    SELECT
        TARGET_COHORT_ID,
        OUTCOME_COHORT_ID,
        {@required_benefit_sources != ''} ? {
        COUNT (
            DISTINCT CASE
                WHEN calibrated_RR <= @benefit AND calibrated_RR >= @lower_benefit AND DATABASE_ID IN (@required_benefit_sources)
                THEN DATABASE_ID
            END
        ) as required_source_count,
        }
        COUNT (
            DISTINCT CASE
                WHEN calibrated_RR <= @benefit AND calibrated_RR >= @lower_benefit AND  DATABASE_ID != 'meta-analysis'
                THEN DATABASE_ID
            END
        ) as benefit_count,
         COUNT (
            DISTINCT CASE
                WHEN calibrated_RR >= @risk AND calibrated_RR >= @lower_benefit AND  DATABASE_ID != 'meta-analysis'
                THEN DATABASE_ID
            END
        ) as risk_count
    FROM @schema.scc_result
    WHERE scc_result.analysis_id = @analysis_id
    AND calibrated_P_VALUE < @p_cut_value
    GROUP BY TARGET_COHORT_ID, OUTCOME_COHORT_ID
)
--{@excluded_concepts != '' & @vocabulary_schema != ''} ? {
--, excluded_cohort_list AS (
--    SELECT DISTINCT cohort_definition_id FROM @schema.cg_cohort_concept_set ccs
--    INNER JOIN @vocabulary_schema.concept_ancestor ca ON ca.descendant_concept_id = ccs.concept_id
--    WHERE ca.ancestor_concept_id IN (@excluded_concepts)
--)
--}
SELECT
    fr.TARGET_COHORT_ID,
    t.cohort_name as TARGET_COHORT_NAME,
    fr.OUTCOME_COHORT_ID,
    o.cohort_name AS OUTCOME_COHORT_NAME,

    COALESCE (benefit_t.benefit_count, 0) AS benefit_count,
    COALESCE (benefit_t.risk_count, 0) risk_count,
    mr.I2 as I2,
    -- {@show_exposure_classes}?{STRING_AGG(distinct ec.EXPOSURE_CLASS_NAME, ';') as ECN,}
    ROUND(mr.calibrated_RR, 2) as meta_RR,
    ROUND(mr.calibrated_P_VALUE) as p
FROM @schema.scc_result fr

    LEFT JOIN benefit_t ON benefit_t.TARGET_COHORT_ID = fr.TARGET_COHORT_ID AND benefit_t.OUTCOME_COHORT_ID = fr.OUTCOME_COHORT_ID
    INNER JOIN @schema.cg_cohort_definition t ON t.cohort_definition_id = fr.target_cohort_id
    INNER JOIN @schema.cg_cohort_definition o ON o.cohort_definition_id = fr.outcome_cohort_id

    LEFT JOIN @schema.scc_result mr ON (
        fr.outcome_cohort_id = mr.outcome_cohort_id AND
        fr.target_cohort_id = mr.target_cohort_id AND
        mr.DATABASE_ID = 'meta-analysis'
        AND mr.analysis_id = @analysis_id
    )
--    {@show_exposure_classes}?{
--    INNER JOIN @schema.cohort_exposure_class tec ON tec.cohort_definition_id = t.cohort_definition_id
--    INNER JOIN @schema.exposure_class ec ON ec.exposure_class_id = tec.exposure_class_id
--    }

    WHERE 1 = 1
--    {@excluded_concepts != '' & @vocabulary_schema != ''} ? { AND
--        {@show_exposure_classes} ? {t.cohort_definition_id} : {o.cohort_definition_id} NOT IN
--        (
--            SELECT DISTINCT cohort_definition_id FROM @schema.cohort_concept_set ccs
--            INNER JOIN @vocabulary_schema.concept_ancestor ca ON ca.descendant_concept_id = ccs.concept_id
--            WHERE ca.ancestor_concept_id IN (@excluded_concepts)
--        )
--    }
    {@outcome_cohorts != ''} ? {AND o.cohort_definition_id IN (@outcome_cohorts)}
    {@excluded_outcome_cohorts != ''} ? {AND o.cohort_definition_id NOT IN (@excluded_outcome_cohorts)}
    {@target_cohorts != ''} ? {AND t.cohort_definition_id IN (@target_cohorts)}
    {@excluded_target_cohorts != ''} ? {AND t.cohort_definition_id NOT IN (@excluded_target_cohorts)}
    --{@show_exposure_classes & @exposure_classes != ''} ? {AND ec.EXPOSURE_CLASS_NAME IN (@exposure_classes)}

   {@filter_by_meta_analysis} ? {
       AND mr.calibrated_RR <= @benefit AND mr.calibrated_RR >= @lower_benefit AND mr.calibrated_P_VALUE < @p_cut_value
    } : {
    AND COALESCE (benefit_t.benefit_count, 0) >= @benefit_count
    }
    AND COALESCE (benefit_t.risk_count, 0) < @risk_count + 1
    {@required_benefit_sources != ''} ? {AND COALESCE (benefit_t.required_source_count, 0) >= @required_benefit_count}
    {@outcome_search_text != ''} ? {AND lower(o.cohort_name) like lower('%@outcome_search_text%')}
    {@target_search_text != ''} ? {AND lower(t.cohort_name) like lower('%@target_search_text%') }
    GROUP BY fr.target_cohort_id, fr.outcome_cohort_id, t.cohort_name, o.cohort_name, benefit_t.risk_count, benefit_t.benefit_count, mr.I2, mr.calibrated_RR, mr.calibrated_p_value
    {@order_by != ''} ? {ORDER BY @order_by @ascending}
    {@limit != ''} ? {LIMIT @limit {@offset != ''} ? {OFFSET @offset} }
