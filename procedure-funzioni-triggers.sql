CREATE OR REPLACE FUNCTION inserimento_articolo()
    RETURNS trigger
    LANGUAGE plpgsql
    AS $function$
        BEGIN
			NEW.data_creazione := NOW();
            RETURN NEW;
        END;
    $function$;


CREATE OR REPLACE TRIGGER creazione_articolo BEFORE
INSERT ON articoli FOR EACH ROW EXECUTE FUNCTION inserimento_articolo();


CREATE OR REPLACE FUNCTION inserimento_contesto()
    RETURNS trigger
    LANGUAGE plpgsql
    AS $function$
        DECLARE autore_articolo_contesto articoli.autore_articolo%TYPE;
        articolo_originale articoli.titolo%TYPE := (get_articolo_from_frase(NEW.testo_frase));
        posizione_partenza_spostamento INTEGER;
        posizione_massima INTEGER := (SELECT COUNT(*) FROM get_modifiche_su_articolo(articolo_originale)
                                      WHERE accettazione = true AND
                                      testo_frase != NEW.testo_frase AND
                                      data_aggiornamento = get_max_data_aggiornamento(testo_frase));
        BEGIN
            IF NEW.posizione < -1
                THEN RAISE EXCEPTION 'La posizione di una frase non può essere minore di -1';
            END IF;

            CALL controlli_contesti_frasi_base(NEW);
            IF NEW.posizione > posizione_massima
            THEN
                RAISE EXCEPTION 'Posizione contesto > posizione massima possibile per questo articolo';
            END IF;


            NEW.accettazione := false;
            autore_articolo_contesto  := (SELECT autore_articolo FROM articoli
                                    INNER JOIN testi_frasi
                                    ON articoli.titolo = testi_frasi.articolo_contenitore
                                    WHERE testi_frasi.id_testo_frase = NEW.testo_frase);
            IF NEW.autore_contesto = autore_articolo_contesto
            THEN
                IF NOT get_existence_contesti_frasi_in_attesa_in_articolo (NOW()::timestamp, articolo_originale) THEN
                    
                    IF NEW.posizione = -1
                    THEN
                        posizione_partenza_spostamento :=
                            (SELECT posizione FROM contesti_frasi
                            WHERE testo_frase = NEW.testo_frase AND
                            accettazione = true AND
                            data_aggiornamento = get_max_data_aggiornamento(NEW.testo_frase));
                    ELSE
                        posizione_partenza_spostamento := NEW.posizione;
                    END IF;
                    
                    NEW.data_revisione_testo := NOW();
                    NEW.data_aggiornamento := NOW();
                    NEW.accettazione := true;

                    CALL spostamento_contesti_frasi (CASE WHEN NEW.posizione = -1 THEN -1 ELSE 1 END,
                        articolo_originale,
                        NEW.testo_frase,
                        posizione_partenza_spostamento);

                    END IF;
                ELSE
                    IF NEW.accettazione = false
                    THEN
                        NEW.data_revisione_testo := NULL;
                        NEW.data_aggiornamento := NULL;
                    END IF;
                END IF;
                NEW.data_creazione := NOW();
                RETURN NEW;
        END;
    $function$;


CREATE OR REPLACE TRIGGER creazione_contesto BEFORE
INSERT ON contesti_frasi FOR EACH ROW EXECUTE FUNCTION inserimento_contesto();



CREATE OR REPLACE FUNCTION aggiornamento_contesto() RETURNS trigger
LANGUAGE plpgsql
AS $function$
    DECLARE
        steps INTEGER := 1;
        old_posizione INTEGER;
        articolo_riferimento testi_frasi.articolo_contenitore%TYPE;
        riga_merge merge_modifiche%ROWTYPE;
        old_data_revisione contesti_frasi.data_revisione_testo%TYPE;
        data_aggiornamento_mod_originale contesti_frasi.data_aggiornamento%TYPE := NOW();
        varianza_posizione_contesto INTEGER;
        addendo_posizione INTEGER;
        next_contesto contesti_frasi.id_contesto%TYPE := (SELECT tjf.id_contesto 
                                                            FROM testi_join_contesti tjf 
                                                            WHERE tjf.id_contesto > NEW.id_contesto
                                                            AND tjf.posizione = NEW.posizione
                                                            ORDER BY tjf.id_contesto DESC
                                                            LIMIT 1);
    BEGIN
        IF is_contesto_revisionato(OLD, NEW)
        THEN
            RAISE EXCEPTION 'contesto già revisionato/non modificabile';
        END IF;

        articolo_riferimento := get_articolo_from_frase(OLD.testo_frase);

        IF get_existence_contesti_frasi_in_attesa_in_articolo (OLD.data_creazione, articolo_riferimento) THEN
            RAISE EXCEPTION 'Altre contesti_frasi da revisionare per questo testo';
        END IF;


        SELECT * INTO riga_merge
        FROM merge_modifiche
        WHERE contesto_da_ordinare = OLD.id_contesto;
        
        CALL controlli_contesti_frasi_base(NEW);
        old_data_revisione := (SELECT data_revisione_testo
                                  FROM contesti_frasi
                                  WHERE testo_frase = NEW.testo_frase AND
                                  accettazione = true AND
                                  data_aggiornamento = get_max_data_aggiornamento(NEW.testo_frase));
        
        varianza_posizione_contesto := calc_varianza_posizione_contesto_frasi(old_posizione, NEW.data_creazione, articolo_riferimento);

        data_aggiornamento_mod_originale :=
            CASE WHEN varianza_posizione_contesto != 0
            THEN
            (NOW() - INTERVAL '1 millisecond')
            ELSE
            NOW()
            END;    
        IF old_data_revisione IS NULL
            THEN
                NEW.data_revisione_testo := data_aggiornamento_mod_originale;
            ELSE
                NEW.data_revisione_testo := old_data_revisione;
            END IF;
        NEW.data_aggiornamento := data_aggiornamento_mod_originale;
        
        IF NEW.accettazione = TRUE
        THEN
            IF get_count_modifiche_in_stessa_posizione(OLD.id_contesto) > 0 AND NEW.accettazione = TRUE
            THEN
                IF riga_merge IS NULL
                THEN 
                    RAISE EXCEPTION 'Frase non ordinabile (manca la necessaria entry in merge_modifiche)';
                END IF;
                IF riga_merge.visionato = FALSE
                THEN
                    RAISE EXCEPTION 'Frase non ordinata';
                END IF;
            END IF;
            
            old_posizione := CASE WHEN NEW.posizione = -1 THEN 
                            get_ultima_posizione_positiva(NEW.testo_frase, NOW()::timestamp - INTERVAL '1 microsecond')
                            ELSE NEW.posizione
                            END;

            addendo_posizione := (varianza_posizione_contesto -
                          (get_contesti_frasi_in_stessa_posizione(old_posizione, NEW.data_creazione, articolo_riferimento) - riga_merge.offset_posizione));

            RAISE NOTICE 'old_posizione = %', old_posizione;

        
            RAISE NOTICE 'addendo_posizione: % - (% - %) = %', varianza_posizione_contesto,
                get_contesti_frasi_in_stessa_posizione(old_posizione, NEW.data_creazione, articolo_riferimento), riga_merge.offset_posizione,
                addendo_posizione;

            IF (NEW.posizione = -1) THEN
                steps := -1;
            ELSE

                DROP TRIGGER IF EXISTS creazione_contesto ON contesti_frasi;

                IF varianza_posizione_contesto != 0
                THEN
                    INSERT INTO contesti_frasi (
                        data_creazione,
                        posizione,
                        accettazione,
                        data_revisione_testo,
                        data_aggiornamento,
                        autore_contesto,
                        testo_frase,
                        collegamento
                    ) VALUES (
                        NEW.data_creazione,
                        NEW.posizione + addendo_posizione,
                        NEW.accettazione,
                        NEW.data_revisione_testo,
                        NOW(),
                        NEW.autore_contesto,
                        NEW.testo_frase,
                        NEW.collegamento
                    );
                END IF;

                CREATE OR REPLACE TRIGGER creazione_contesto BEFORE
                INSERT ON contesti_frasi FOR EACH ROW EXECUTE FUNCTION inserimento_contesto();
            END IF;
            
        
        DELETE FROM merge_modifiche WHERE contesto_da_ordinare = NEW.id_contesto;
        END IF;


        CALL spostamento_contesti_frasi(steps + riga_merge.offset_posizione,
            articolo_riferimento,
            NEW.testo_frase,
            old_posizione + addendo_posizione);
        IF next_contesto > NEW.id_contesto AND next_contesto IS NOT NULL
        THEN
            INSERT INTO merge_modifiche 
            (contesto_da_ordinare)
            VALUES  
            (next_contesto);
        END IF;
        RETURN NEW;
    END;
$function$;

CREATE OR REPLACE TRIGGER revisione_contesti_frasi BEFORE
UPDATE ON contesti_frasi FOR EACH ROW EXECUTE FUNCTION aggiornamento_contesto();




CREATE OR REPLACE FUNCTION inserimento_merge_modifiche ()
    RETURNS trigger
    LANGUAGE plpgsql
    AS $function$
	BEGIN
        NEW.offset_posizione := 0;
        NEW.visionato := false;
        RETURN NEW;
	END;
    $function$;

CREATE OR REPLACE TRIGGER creazione_merge_modifiche BEFORE
INSERT ON merge_modifiche FOR EACH ROW EXECUTE FUNCTION inserimento_merge_modifiche();




CREATE OR REPLACE FUNCTION aggiornamento_merge_modifiche ()
    RETURNS trigger
    LANGUAGE plpgsql
    AS $function$
    BEGIN
        IF NEW.visionato = FALSE
        THEN RAISE EXCEPTION 'visionato non può essere false';
        END IF;

        IF NEW.offset_posizione < 0 OR
            NEW.offset_posizione > get_count_modifiche_in_stessa_posizione(NEW.contesto_da_ordinare)
        THEN
            RAISE EXCEPTION 'non si può avere offset_posizione minore di 0 o maggiore delle contesti_frasi accettate destinate a quella posizione';
        END IF;
        RETURN NEW;
    END;
    $function$;

CREATE OR REPLACE TRIGGER revisione_merge_modifiche BEFORE
UPDATE ON merge_modifiche FOR EACH ROW EXECUTE FUNCTION aggiornamento_merge_modifiche();

CREATE OR REPLACE PROCEDURE spostamento_contesti_frasi (offset_posizione INTEGER, articolo VARCHAR(255), frase_pivot INTEGER, posizione_partenza INTEGER)
LANGUAGE plpgsql
AS $procedure$
    DECLARE
        cursore_contesti_frasi CURSOR FOR (
            SELECT id_contesto, data_creazione, posizione, accettazione, data_aggiornamento, autore_contesto, testo_frase, collegamento, data_revisione_testo
            FROM get_modifiche_su_articolo(articolo)
            WHERE accettazione = TRUE AND
            id_testo_frase != frase_pivot AND
            posizione >= posizione_partenza AND
            data_aggiornamento = (
			  	SELECT MAX(cf.data_aggiornamento)
			  	FROM contesti_frasi cf
			  	WHERE cf.testo_frase = testo_frase AND
                accettazione = true
			)

        );
    BEGIN
        DROP TRIGGER IF EXISTS creazione_contesto ON contesti_frasi;

        FOR contesto_corrente IN cursore_contesti_frasi LOOP
            INSERT INTO contesti_frasi (
                data_creazione,
                posizione,
                accettazione,
                data_revisione_testo,
                data_aggiornamento,
                autore_contesto,
                testo_frase,
                collegamento
            ) VALUES (
                NOW(),
                contesto_corrente.posizione + offset_posizione,
                contesto_corrente.accettazione,
                contesto_corrente.data_revisione_testo,
                NOW(),
                contesto_corrente.autore_contesto,
                contesto_corrente.testo_frase,
                contesto_corrente.collegamento
            );
        END LOOP;

        CREATE OR REPLACE TRIGGER creazione_contesto BEFORE
        INSERT ON contesti_frasi FOR EACH ROW EXECUTE FUNCTION inserimento_contesto();
    END;
$procedure$;


CREATE OR REPLACE FUNCTION is_frase_cancellata (frase INTEGER)
    RETURNS BOOLEAN
    LANGUAGE plpgsql
    as $function$
        BEGIN
        RETURN EXISTS (SELECT * FROM contesti_frasi
        WHERE testo_frase = frase AND
        posizione = -1 AND
        accettazione = true);
        END;
    $function$;

CREATE OR REPLACE FUNCTION contesto_analogo_exists (contesto contesti_frasi)
    RETURNS BOOLEAN
    LANGUAGE plpgsql
    as $function$
        BEGIN
        RETURN EXISTS (
                SELECT * FROM contesti_frasi WHERE
                testo_frase = contesto.testo_frase AND
                posizione = contesto.posizione AND
                accettazione = TRUE AND
                data_aggiornamento = (SELECT MAX(data_aggiornamento) FROM contesti_frasi
                WHERE testo_frase = contesto.testo_frase AND accettazione = TRUE)
            );
        END;
    $function$;


CREATE OR REPLACE FUNCTION is_frase_in_articolo (articolo articoli.titolo%TYPE, frase INTEGER)
    RETURNS BOOLEAN
    LANGUAGE plpgsql
    as $function$
        BEGIN
        IF get_articolo_from_frase(frase) != articolo
        THEN
            RAISE EXCEPTION 'Frase non appartenente all''articolo selezionato';
        END IF;
        RETURN is_frase_in_articolo(frase);
    END;
    $function$;

CREATE OR REPLACE FUNCTION is_frase_in_articolo (frase INTEGER)
    RETURNS BOOLEAN
    LANGUAGE plpgsql
    as $function$
        DECLARE
            articolo articoli.titolo%TYPE := get_articolo_from_frase(frase);
        BEGIN
            RETURN EXISTS (
                SELECT posizione, testo, id_testo_frase, id_contesto
                FROM ricostruzione_articolo(articolo, NOW()::timestamp) 
                WHERE id_testo_frase = frase
            );
        END;
    $function$;

CREATE OR REPLACE FUNCTION calc_varianza_posizione_contesto_frasi (posizione_originale INTEGER,
                                              data_inserimento_contesto contesti_frasi.data_creazione %TYPE,
                                              articolo articoli.titolo %TYPE)
RETURNS INTEGER
LANGUAGE plpgsql
AS $function$
    DECLARE
        id_testo_frase INTEGER := (SELECT testo_frase
                                    FROM get_modifiche_su_articolo(articolo)
                                    WHERE posizione = posizione_originale AND
                                   accettazione = true AND
                                   data_aggiornamento = (SELECT MAX (data_aggiornamento)
                                                         FROM contesti_frasi WHERE data_aggiornamento <
                                                         data_inserimento_contesto AND
                                                         accettazione = true AND
                                                         articolo_contenitore = articolo));


        ultima_posizione_positiva INTEGER := get_ultima_posizione_positiva(id_testo_frase, (SELECT MAX (data_aggiornamento)
                                                                           FROM contesti_frasi WHERE posizione > -1 AND
                                                                           testo_frase = id_testo_frase AND
                                                                           accettazione = true));
    BEGIN
        RETURN CASE WHEN id_testo_frase IS NULL THEN 0 ELSE ultima_posizione_positiva - posizione_originale END;
    END;
$function$;


CREATE OR REPLACE PROCEDURE controlli_contesti_frasi_base (contesto_da_controllare contesti_frasi)
LANGUAGE plpgsql
AS $procedure$
    BEGIN
        IF is_frase_cancellata(contesto_da_controllare.testo_frase)
        THEN 
            RAISE EXCEPTION 'Frase già cancellata';
        END IF;

        IF contesto_da_controllare.posizione > -1 AND is_frase_in_articolo(contesto_da_controllare.testo_frase)
        THEN
            RAISE EXCEPTION 'frase già presente';
        END IF;

        IF contesto_da_controllare.posizione = -1 AND NOT is_frase_in_articolo(contesto_da_controllare.testo_frase)
        THEN
            RAISE EXCEPTION 'frase non cancellabile (non è presente nell''articolo)';
        END IF;

        IF contesto_analogo_exists (contesto_da_controllare.*)
        THEN
            RAISE EXCEPTION 'contesto analogo già inserita';
        END IF;
    END;
$procedure$;

CREATE OR REPLACE FUNCTION get_articolo_from_frase (frase INTEGER) RETURNS articoli.titolo%TYPE
LANGUAGE plpgsql
AS $function$
    BEGIN 
        RETURN (SELECT articolo_contenitore FROM testi_frasi WHERE id_testo_frase = frase);
    END;
$function$;


CREATE OR REPLACE FUNCTION is_contesto_revisionato (old_contesto contesti_frasi, new_contesto contesti_frasi)
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $function$
    BEGIN
    RETURN NOT ((new_contesto.id_contesto = old_contesto.id_contesto AND
                 new_contesto.data_creazione = old_contesto.data_creazione AND
                 new_contesto.posizione = old_contesto.posizione AND
                 new_contesto.autore_contesto = old_contesto.autore_contesto AND
                 new_contesto.testo_frase = old_contesto.testo_frase AND
                 new_contesto.collegamento = old_contesto.collegamento) AND
                old_contesto.data_aggiornamento IS NULL);
    END;
$function$;

CREATE OR REPLACE FUNCTION get_existence_contesti_frasi_in_attesa_in_articolo (data_creazione_minima contesti_frasi.data_creazione%TYPE, articolo_riferimento articoli.titolo%TYPE)
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $function$
    BEGIN
    RETURN EXISTS (SELECT * FROM get_modifiche_su_articolo(articolo_riferimento)
                  WHERE data_creazione < data_creazione_minima AND
                  accettazione = false AND
                  data_aggiornamento IS NULL);
    END;
$function$;

CREATE OR REPLACE FUNCTION get_max_data_aggiornamento(frase INTEGER)
RETURNS contesti_frasi.data_aggiornamento%TYPE
LANGUAGE plpgsql
AS $function$
    BEGIN
        RETURN (SELECT MAX(data_aggiornamento)
                FROM contesti_frasi
                WHERE testo_frase = frase AND
                accettazione = true
                );
    END;
$function$;


CREATE OR REPLACE FUNCTION get_contesti_frasi_in_stessa_posizione(posizione_contesto INTEGER, data_aggiornamento_minima contesti_frasi.data_creazione%TYPE, articolo articoli.titolo%TYPE) --restituisce il numero di contesti_frasi che hanno data_creazione != data_aggiornamento, che sono state effettuate in una posizione arbitraria e create dopo una specifica data
RETURNS INTEGER
LANGUAGE plpgsql
AS $function$
    BEGIN
    RETURN (SELECT COUNT(*) FROM get_modifiche_su_articolo(articolo)
            WHERE data_aggiornamento > data_aggiornamento_minima AND
            accettazione = true AND
            data_creazione != data_aggiornamento AND
            posizione = posizione_contesto
            );
    END;
$function$;


CREATE OR REPLACE FUNCTION get_ultima_posizione_positiva (frase INTEGER, max_data_aggiornamento contesti_frasi.data_aggiornamento%TYPE)
RETURNS INTEGER
LANGUAGE plpgsql
AS $function$
    BEGIN
        RETURN (
                    SELECT posizione
                    FROM contesti_frasi
                    WHERE testo_frase = frase AND 
                    posizione > -1 AND 
                    accettazione = true AND
                    data_aggiornamento <= max_data_aggiornamento
                    ORDER BY data_aggiornamento DESC
                    LIMIT 1
                );
    END;
$function$;

CREATE OR REPLACE FUNCTION calc_rating (autore autori.id_autore%TYPE)
RETURNS DOUBLE PRECISION
LANGUAGE plpgsql
AS $function$
    DECLARE
    numero_articoli INTEGER;
    modifiche_accettate INTEGER;
    modifiche_proposte INTEGER;
    BEGIN
        numero_articoli := (SELECT COUNT(*) FROM articoli WHERE autore_articolo = autore);

        modifiche_proposte := (SELECT COUNT(*) FROM testi_join_contesti as tjf
            INNER JOIN articoli as a ON tjf.articolo_contenitore = a.titolo
            WHERE tjf.autore_contesto = autore AND
            NOT a.autore_articolo = autore AND 
            tjf.data_revisione_testo IS NOT NULL AND 
            (tjf.data_revisione_testo = tjf.data_aggiornamento OR tjf.posizione = -1));

        modifiche_accettate := (SELECT COUNT(*) FROM testi_join_contesti as tjf
            INNER JOIN articoli as a ON tjf.articolo_contenitore = a.titolo
            WHERE tjf.autore_contesto = autore AND
            NOT a.autore_articolo = autore AND 
            tjf.data_revisione_testo IS NOT NULL AND 
            (tjf.data_revisione_testo = tjf.data_aggiornamento OR tjf.posizione = -1) AND
            tjf.accettazione = true);

        IF modifiche_proposte = 0
        THEN
            RETURN 0;
        ELSE
            RETURN numero_articoli * (modifiche_accettate / modifiche_proposte);
        END IF;
    END;
$function$;


CREATE OR REPLACE FUNCTION ricostruzione_articolo (articolo articoli.titolo%TYPE, data_articolo contesti_frasi.data_aggiornamento%TYPE)
RETURNS SETOF testi_join_contesti
LANGUAGE plpgsql
AS $function$
    BEGIN
        RETURN QUERY(SELECT * FROM get_modifiche_su_articolo(articolo)
			WHERE accettazione = TRUE
            AND posizione > -1
            AND data_aggiornamento = (
                SELECT MAX(cf2.data_aggiornamento)
                FROM contesti_frasi cf2
                WHERE cf2.testo_frase = testo_frase AND cf2.data_aggiornamento <= data_articolo
                )
                ORDER BY posizione
        );
    END;
$function$;

CREATE OR REPLACE FUNCTION get_modifiche_su_articolo (articolo articoli.titolo%TYPE)
RETURNS SETOF testi_join_contesti
LANGUAGE plpgsql
AS $function$
    BEGIN
        RETURN QUERY(SELECT * FROM testi_join_contesti
            WHERE articolo_contenitore = articolo
        );
    END;
$function$;

CREATE OR REPLACE FUNCTION get_count_modifiche_in_stessa_posizione (contesto contesti_frasi.id_contesto%TYPE)
RETURNS INTEGER
LANGUAGE plpgsql
AS $function$
    DECLARE 

		data_inserimento_contesto contesti_frasi.data_creazione%TYPE := (SELECT data_creazione 
                                                                        FROM contesti_frasi 
                                                                        WHERE id_contesto = contesto);

        articolo articoli.titolo%TYPE := (SELECT articolo_contenitore 
                    FROM testi_join_contesti tjf 
                    WHERE id_contesto = contesto);
        posizione_originale INTEGER := (SELECT posizione
										FROM contesti_frasi
										WHERE id_contesto = contesto);
    BEGIN
        RETURN (SELECT COUNT(*)
            FROM get_modifiche_su_articolo(articolo)
            WHERE data_aggiornamento >= data_inserimento_contesto AND
            accettazione = true AND
            posizione = posizione_originale);
    END;
$function$;

CREATE OR REPLACE FUNCTION get_modifiche_di_autore (autore autori.id_autore%TYPE)
RETURNS SETOF testi_join_contesti
LANGUAGE plpgsql
AS $function$
    BEGIN
        RETURN QUERY(SELECT * FROM testi_join_contesti
            WHERE autore_contesto = articolo
        );
    END;
$function$;

CREATE OR REPLACE FUNCTION get_storico_articolo_di_autore(autore autori.id_autore%TYPE, articolo articoli.titolo%TYPE)
RETURNS SETOF testi_join_contesti
LANGUAGE plpgsql
AS $function$
    BEGIN
        IF NOT EXISTS (
            SELECT 1 
            FROM articoli a 
            WHERE a.titolo = articolo
        ) THEN
            RAISE EXCEPTION 'L''articolo non esiste';
        END IF;

        IF NOT EXISTS (
            SELECT 1
            FROM autori a
            WHERE a.id_autore = autore
        ) THEN
            RAISE EXCEPTION 'L''autore non esiste';
        END IF;

        IF NOT EXISTS (
            SELECT 1
            FROM articoli a
            WHERE a.titolo = articolo
            AND a.autore_articolo = autore
        ) AND NOT EXISTS (
            SELECT 1
            FROM testi_join_contesti tjc
            WHERE tjc.articolo_contenitore = articolo
            AND tjc.autore_contesto = autore
        ) THEN
            RAISE EXCEPTION 'Articolo non accessibile per l''autore con ID %', autore;
        END IF;

        RETURN QUERY (
            SELECT *
            FROM testi_join_contesti tjc
            WHERE tjc.articolo_contenitore = articolo
            AND tjc.accettazione = TRUE
            ORDER BY tjc.posizione, tjc.data_revisione_testo
        );
    END;
$function$;

CREATE OR REPLACE FUNCTION crea_testo_frase (testo_frase testi_frasi.testo%TYPE, articolo_di_appartenenza testi_frasi.articolo_contenitore%TYPE)
RETURNS testi_frasi.id_testo_frase%TYPE
LANGUAGE plpgsql
AS $function$
    DECLARE id_testo testi_frasi.id_testo_frase%TYPE;
    BEGIN
        INSERT INTO testi_frasi
        (testo, articolo_contenitore)
        VALUES
        (testo_frase, articolo_di_appartenenza)
        RETURNING id_testo_frase INTO id_testo;
        RETURN id_testo;
    END;
$function$;


CREATE OR REPLACE FUNCTION crea_contesto (frase testi_frasi.id_testo_frase%TYPE, posizione_contesto contesti_frasi.posizione%TYPE, autore contesti_frasi.autore_contesto%TYPE)
RETURNS contesti_frasi.id_contesto%TYPE
LANGUAGE plpgsql
AS $function$
    DECLARE
        id_contesto_frase contesti_frasi.id_contesto%TYPE;
    BEGIN
        INSERT INTO contesti_frasi
        (testo_frase, posizione, autore_contesto)
        VALUES
        (frase, posizione_contesto, autore)
        RETURNING id_contesto INTO id_contesto_frase;

        RETURN id_contesto_frase;
    END;
$function$;


CREATE OR REPLACE PROCEDURE crea_frase_con_contesto (testo_frase testi_frasi.testo%TYPE, articolo_di_appartenenza testi_frasi.articolo_contenitore%TYPE, posizione_contesto contesti_frasi.posizione%TYPE, autore contesti_frasi.autore_contesto%TYPE)
LANGUAGE plpgsql
AS $procedure$
	DECLARE id_testo_frase INT;
	id_contesto_frase INT;
    BEGIN
		id_testo_frase = crea_testo_frase (testo_frase, articolo_di_appartenenza);
        id_contesto_frase = crea_contesto(id_testo_frase, posizione_contesto, autore);
    END;
$procedure$;

CREATE OR REPLACE PROCEDURE registra_autore (nickname_autore autori.nickname%TYPE, password_autore autori.password%TYPE, email_autore autori.email%TYPE)
LANGUAGE plpgsql
AS $procedure$
    BEGIN
        INSERT INTO autori 
        (nickname, password, email)
        VALUES
        (nickname_autore, password_autore, email_autore);
    END;
$procedure$;

CREATE OR REPLACE FUNCTION accesso_nickname (nickname_autore autori.nickname%TYPE, password_autore autori.password%TYPE)
RETURNS INTEGER
LANGUAGE plpgsql
AS $function$
    BEGIN
        RETURN (SELECT id_autore 
            FROM autori
            WHERE nickname = nickname_autore AND password = password_autore
        );
    END;
$function$;

CREATE OR REPLACE FUNCTION accesso_email (email_autore autori.email%TYPE, password_autore autori.password%TYPE)
RETURNS INTEGER
LANGUAGE plpgsql
AS $function$
    BEGIN
        RETURN (SELECT id_autore 
            FROM autori
            WHERE email = email_autore AND password = password_autore
        );
    END;
$function$;
