{DEFAULT @limit = ''}
{DEFAULT @offset = ''}
{DEFAULT @order_by = ''}
{DEFAULT @ascending = 'ASC'}
{DEFAULT @required_benefit_sources = ''}
 -- Should always be the same as number of data sources
{DEFAULT @required_benefit_count = 0}
{DEFAULT @show_exposure_classes = FALSE}
{DEFAULT @outcome_cohort_length = FALSE}
{DEFAULT @target_cohort_length = TRUE}
{DEFAULT @target_cohorts = (1, 2, 3)}
{DEFAULT @filter_by_meta_analysis = FALSE}
{DEFAULT @filter_outcome_types = FALSE}

WITH benefit_t AS(
    SELECT TARGET_COHORT_ID, OUTCOME_COHORT_ID, COUNT(DISTINCT(DATABASE_ID)) AS THRESH_COUNT
    FROM @schema.scc_result
    WHERE calibrated_RR <= @benefit
        AND calibrated_RR >= @lower_benefit
        AND calibrated_P_VALUE < @p_cut_value
        AND DATABASE_ID != 'meta-analysis'
    GROUP BY TARGET_COHORT_ID, OUTCOME_COHORT_ID
),

risk_t AS (
    SELECT TARGET_COHORT_ID, OUTCOME_COHORT_ID, COUNT(DISTINCT(DATABASE_ID)) AS THRESH_COUNT
    FROM @schema.scc_result
    WHERE calibrated_RR >= @risk
        AND calibrated_RR >= @lower_benefit
        AND calibrated_P_VALUE < @p_cut_value
        AND DATABASE_ID != 'meta-analysis'
    GROUP BY TARGET_COHORT_ID, OUTCOME_COHORT_ID
)
{@required_benefit_sources != ''} ? {,
-- inner join to results that are from required data sources only
req_benefit_sources AS (
    SELECT TARGET_COHORT_ID, OUTCOME_COHORT_ID, COUNT(DISTINCT(DATABASE_ID)) AS required_count
    FROM @schema.scc_result
    WHERE calibrated_RR <= @benefit
        AND calibrated_RR >= @lower_benefit
        AND calibrated_P_VALUE < @p_cut_value
        AND DATABASE_ID IN (@required_benefit_sources)
    GROUP BY TARGET_COHORT_ID, OUTCOME_COHORT_ID
)
}

SELECT
    fr.TARGET_COHORT_ID,
    t.cohort_name as TARGET_COHORT_NAME,
    fr.OUTCOME_COHORT_ID,
    o.cohort_name AS OUTCOME_COHORT_NAME,

    CASE
        WHEN risk_t.THRESH_COUNT IS NULL THEN 0
        ELSE risk_t.THRESH_COUNT
    END AS risk_count,

    CASE
        WHEN benefit_t.THRESH_COUNT IS NULL THEN 0
        ELSE benefit_t.THRESH_COUNT
    END AS benefit_count,
    mr2.I2 as I2,
    -- {@show_exposure_classes}?{STRING_AGG(distinct ec.EXPOSURE_CLASS_NAME, ';') as ECN,}
    ROUND(mr.calibrated_RR, 2) as meta_RR,
    ROUND(mr.calibrated_P_VALUE) as p
FROM @schema.scc_result fr

    LEFT JOIN benefit_t ON benefit_t.TARGET_COHORT_ID = fr.TARGET_COHORT_ID AND benefit_t.OUTCOME_COHORT_ID = fr.OUTCOME_COHORT_ID
    LEFT JOIN risk_t ON risk_t.TARGET_COHORT_ID = fr.TARGET_COHORT_ID AND risk_t.OUTCOME_COHORT_ID = fr.OUTCOME_COHORT_ID

    {@required_benefit_sources != ''} ?{
    LEFT JOIN req_benefit_sources rbs ON rbs.TARGET_COHORT_ID = fr.TARGET_COHORT_ID AND rbs.OUTCOME_COHORT_ID = fr.OUTCOME_COHORT_ID
    }

    INNER JOIN @schema.cg_cohort_definition t ON t.cohort_definition_id = fr.target_cohort_id
    INNER JOIN @schema.cg_cohort_definition o ON o.cohort_definition_id = fr.outcome_cohort_id
    INNER JOIN @schema.scc_outcome_exposure oc ON o.cohort_definition_id = oc.outcome_cohort_id

    LEFT JOIN @schema.scc_result mr ON (
        fr.outcome_cohort_id = mr.outcome_cohort_id AND
        fr.target_cohort_id = mr.target_cohort_id AND
        mr.DATABASE_ID = 'meta-analysis'
    )
    LEFT JOIN @schema.scc_result mr2 ON (
        fr.outcome_cohort_id = mr2.outcome_cohort_id AND
        fr.target_cohort_id = mr2.target_cohort_id AND
        mr2.DATABASE_ID = 'meta-analysis'
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
    {@outcome_cohort_length} ? {AND o.cohort_definition_id IN (@outcome_cohorts)}
    {@target_cohort_length} ? {AND t.cohort_definition_id IN (@target_cohorts)}
    --{@show_exposure_classes & @exposure_classes != ''} ? {AND ec.EXPOSURE_CLASS_NAME IN (@exposure_classes)}

       {@filter_by_meta_analysis} ? {
       AND mr.calibrated_RR <= @benefit AND mr.calibrated_RR >= @lower_benefit AND mr.calibrated_P_VALUE < @p_cut_value
    } : {
    AND 1 = CASE
        WHEN benefit_t.THRESH_COUNT IS NULL AND @benefit_count = 0 THEN 1
        WHEN benefit_t.THRESH_COUNT >= @benefit_count THEN 1
        ELSE 0
    END
    }
    AND 1 = CASE
        WHEN risk_t.THRESH_COUNT IS NULL AND @risk_count = 0 THEN 1
        WHEN risk_t.THRESH_COUNT > @risk_count THEN 0
        ELSE 1
    END
    {@required_benefit_sources != ''} ? {AND rbs.required_count >= @required_benefit_count}
    -- {@filter_outcome_types} ? {AND oc.outcome_type IN (@outcome_types)}
    GROUP BY fr.target_cohort_id, fr.outcome_cohort_id, t.cohort_name, o.cohort_name, risk_t.THRESH_COUNT, benefit_t.THRESH_COUNT, mr2.I2, mr.calibrated_RR, mr.calibrated_p_value
    {@order_by != ''} ? {ORDER BY @order_by @ascending}
    {@limit != ''} ? {LIMIT @limit {@offset != ''} ? {OFFSET @offset} }
