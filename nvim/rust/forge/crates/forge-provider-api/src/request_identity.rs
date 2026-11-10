//! Complete immutable replay keys which do not retain callback or resource ownership.

use crate::{control::ControlToolDefinition, trace::ProviderTrace, *};
use std::sync::{Arc, Weak};

struct AuthorityIdentity<T: ?Sized>(Weak<T>);
impl<T: ?Sized> AuthorityIdentity<T> {
    /// Capture allocation identity while allowing the underlying authority value to be dropped.
    fn new(authority: &Arc<T>) -> Self {
        Self(Arc::downgrade(authority))
    }
}
impl<T: ?Sized> Clone for AuthorityIdentity<T> {
    /// Clone metadata without reacquiring resource ownership.
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<T: ?Sized> PartialEq for AuthorityIdentity<T> {
    /// Compare original allocation identity even after every strong owner has released it.
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }
}
impl<T: ?Sized> Eq for AuthorityIdentity<T> {}

/// Retain every immutable open value and weak authority identity for exact operation replay.
/// This key owns normalized values, including secret-bearing launch configuration, and must
/// never be logged. Cached keys do not extend resource leases, trace owners, or callback lives.
/// Weak allocation identity remains comparable after owners are released; the key cannot
/// upgrade or exercise that authority. The enclosing retained record determines key lifetime.
#[derive(Clone, Eq, PartialEq)]
pub struct SessionRequestIdentity {
    thread_id: crate::ThreadId,
    mode: SessionMode,
    workspace: String,
    history: SessionHistory,
    configuration: SessionConfiguration,
    resource_lease: Option<AuthorityIdentity<dyn ResourceLease>>,
    tools: Vec<ControlToolDefinition>,
    tool_endpoint: Option<HostToolEndpoint>,
    system_instructions: String,
    trace: AuthorityIdentity<dyn ProviderTrace>,
}
impl From<&SessionRequest> for SessionRequestIdentity {
    /// Snapshot the complete open request without retaining live resource owners.
    fn from(request: &SessionRequest) -> Self {
        let SessionRequest {
            thread_id,
            mode,
            workspace,
            history,
            configuration,
            resource_lease,
            tools,
            tool_endpoint,
            system_instructions,
            trace,
        } = request;
        Self {
            thread_id: thread_id.clone(),
            mode: *mode,
            workspace: workspace.clone(),
            history: history.clone(),
            configuration: configuration.clone(),
            resource_lease: resource_lease.as_ref().map(AuthorityIdentity::new),
            tools: tools.clone(),
            tool_endpoint: tool_endpoint.clone(),
            system_instructions: system_instructions.clone(),
            trace: AuthorityIdentity::new(trace),
        }
    }
}

/// Retain complete immutable input values and weak identities of invocation authority.
/// Cache this key instead of PromptRequest so replay metadata cannot keep receipt senders,
/// tool endpoints, permission owners, resource leases, or host-effect owners alive. Weak
/// dispatch identity likewise cannot keep the handoff watch sender alive or hide its loss.
/// The values include input and credentials and must not be logged or treated as authority.
#[derive(Clone, Eq, PartialEq)]
pub struct PromptRequestIdentity {
    thread_id: crate::ThreadId,
    turn_id: crate::TurnId,
    scope: InvocationScope,
    execution: ExecutionMode,
    configuration: InvocationConfiguration,
    tools: Vec<ControlToolDefinition>,
    tool_endpoint: Option<HostToolEndpoint>,
    resource_lease: Option<AuthorityIdentity<dyn ResourceLease>>,
    submission_id: SubmissionId,
    attempt_id: DeliveryAttemptId,
    input: crate::forge::TurnInput,
    dispatch: submission::DispatchIdentity,
    recovered_inputs: Vec<RecoveredInput>,
    context: crate::forge::TurnContext,
    attachments: Vec<FileAttachment>,
    receipt: AuthorityIdentity<dyn submission::InputReceipt>,
    permissions: AuthorityIdentity<dyn permissions::ProviderPermissions>,
    provider_tools: AuthorityIdentity<dyn control::ProviderTools>,
    turn_control: AuthorityIdentity<dyn turn_control::TurnControl>,
    host_effects: AuthorityIdentity<dyn ProviderHostEffects>,
}
impl From<&PromptRequest> for PromptRequestIdentity {
    /// Snapshot every input and authority field, making future fields a compile-time obligation.
    fn from(request: &PromptRequest) -> Self {
        let PromptRequest {
            thread_id,
            turn_id,
            scope,
            execution,
            configuration,
            tools,
            tool_endpoint,
            resource_lease,
            submission_id,
            attempt_id,
            input,
            dispatch,
            recovered_inputs,
            context,
            attachments,
            receipt,
            permissions,
            provider_tools,
            turn_control,
            host_effects,
        } = request;
        Self {
            thread_id: thread_id.clone(),
            turn_id: turn_id.clone(),
            scope: scope.clone(),
            execution: *execution,
            configuration: configuration.clone(),
            tools: tools.clone(),
            tool_endpoint: tool_endpoint.clone(),
            resource_lease: resource_lease.as_ref().map(AuthorityIdentity::new),
            submission_id: submission_id.clone(),
            attempt_id: attempt_id.clone(),
            input: input.clone(),
            dispatch: dispatch.identity(),
            recovered_inputs: recovered_inputs.clone(),
            context: context.clone(),
            attachments: attachments.clone(),
            receipt: AuthorityIdentity::new(receipt),
            permissions: AuthorityIdentity::new(permissions),
            provider_tools: AuthorityIdentity::new(provider_tools),
            turn_control: AuthorityIdentity::new(turn_control),
            host_effects: AuthorityIdentity::new(host_effects),
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
enum SteeringDeliveryIdentity {
    Journaled {
        submission_id: SubmissionId,
        receipt: AuthorityIdentity<dyn submission::InputReceipt>,
    },
    Context,
}

/// Retain exact steering values and weak journal authority without inventing notification IDs.
/// The receipt instance binds one immutable submission/attempt. Releasing every strong receipt
/// owner leaves comparable allocation identity, not permission to commit another journal write.
#[derive(Clone, Eq, PartialEq)]
pub struct SteeringRequestIdentity {
    scope: InvocationScope,
    delivery: SteeringDeliveryIdentity,
    input: crate::forge::TurnInput,
    attachments: Vec<FileAttachment>,
}
impl From<&SteeringRequest> for SteeringRequestIdentity {
    /// Snapshot the exact target and input without extending the receipt's lifetime.
    fn from(request: &SteeringRequest) -> Self {
        let SteeringRequest {
            scope,
            delivery,
            input,
            attachments,
        } = request;
        let delivery = match delivery {
            SteeringDelivery::Journaled { receipt } => SteeringDeliveryIdentity::Journaled {
                submission_id: receipt.id().clone(),
                receipt: AuthorityIdentity::new(receipt),
            },
            SteeringDelivery::Context => SteeringDeliveryIdentity::Context,
        };
        Self {
            scope: scope.clone(),
            delivery,
            input: input.clone(),
            attachments: attachments.clone(),
        }
    }
}
