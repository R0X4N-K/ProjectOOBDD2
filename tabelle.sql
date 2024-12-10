CREATE TABLE articoli (
    titolo VARCHAR(255) NOT NULL,
    autore_articolo INT,
    data_creazione TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

CREATE TABLE autori (
    nickname VARCHAR(255) NOT NULL,
    password VARCHAR(512) NOT NULL,
    rating DOUBLE PRECISION,
    email VARCHAR(255) NOT NULL,
    id_autore INT NOT NULL
);

CREATE TABLE testi_frasi (
	id_testo_frase INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	testo VARCHAR (255),
	articolo_contenitore VARCHAR(255) NOT NULL,

	FOREIGN KEY (articolo_contenitore) REFERENCES articoli(titolo),

	CHECK (NOT TRIM(BOTH ' ' FROM testo) = '')
);

CREATE TABLE contesti_frasi (
	id_contesto INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	data_creazione TIMESTAMP WITHOUT TIME ZONE NOT NULL,
	posizione INT NOT NULL,
	accettazione BOOL DEFAULT false NOT NULL,
	data_accettazione_testo TIMESTAMP WITHOUT TIME ZONE,
	data_aggiornamento TIMESTAMP WITHOUT TIME ZONE,
	autore_contesto INT NOT NULL,
	testo_frase INT NOT NULL,
	collegamento VARCHAR(255) DEFAULT NULL,
	FOREIGN KEY (autore_contesto) REFERENCES autori(id_autore),
	FOREIGN KEY (testo_frase) REFERENCES testi_frasi(id_testo_frase),
	FOREIGN KEY (collegamento) REFERENCES articoli(titolo),

	CHECK (data_creazione < data_revisione)
);


CREATE TABLE merge_modifiche (
	contesto_da_ordinare INT,
	offset_posizione INT,

	PRIMARY KEY (contesto_da_ordinare),
	FOREIGN KEY (contesto_da_ordinare) REFERENCES contesti_frasi (id_contesto)
);
