SELECT *
FROM events.event, authorization.ClientSession, events.inquiry
WHERE (event.sessionId = ClientSession.sessionId 
AND inquiry.eventId = event.eventId);

-- ORDER BY sessionEnd DESC

SELECT * FROM authorization.ClientSession;
SELECT * FROM events.event;