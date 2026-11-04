/// Overlapping state observers wait for the previous lease without acknowledging its output.
#[tokio::test]
async fn owner_reconnect_serializes_overlapping_snapshot_leases() -> Result<()> {
    let fixture = owner_transition_fixture().await?;
    let first = fixture
        .controller
        .exchange_owner
        .reconnect_snapshot(None, std::num::NonZeroU16::new(256).unwrap())
        .await?;
    let (first_state, first_confirmation) = first.into_parts();
    let mut second = Box::pin(
        fixture
            .controller
            .exchange_owner
            .reconnect_snapshot(None, std::num::NonZeroU16::new(256).unwrap()),
    );
    assert!(futures_util::poll!(second.as_mut()).is_pending());
    fixture.controller.publications.publish(
        forge_protocol::message::SessionEvent {
            session_id: fixture.session.clone(),
            event: "exchange_updated".into(),
            payload: json!({"text":"overlap_marker"}),
        },
        None,
    )?;
    drop(first_confirmation);
    let second = tokio::time::timeout(Duration::from_secs(5), second).await??;
    let (second_state, second_confirmation) = second.into_parts();
    assert!(
        second_state["owner_publication"]["high_water"]
            .as_u64()
            .unwrap()
            > first_state["owner_publication"]["high_water"]
                .as_u64()
                .unwrap()
    );
    assert!(
        second_state["owner_publication"]["events"]
            .as_array()
            .unwrap()
            .iter()
            .any(|record| record["event"]["event"] == "exchange_updated"
                && record["event"]["payload"]["text"] == "overlap_marker")
    );
    drop(second_confirmation);
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}
