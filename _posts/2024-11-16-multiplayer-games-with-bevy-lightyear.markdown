--- 
layout: post
published: false
title: "Actually writing a game.."
permalink: /article/bevy-multiplayer-games-with-lightyear
tags: 
- gamedev
- netcode
- multiplayer
---

So what do the multiplayer bits look like in a bevy game that uses Lightyear?

Entities that are replicated to clients tend to be either interpolated or predicted.

Interpolated means you're seeing the entity between the two most recent server snapshots of its position.
Predicted means the client has simulated the position ahead of the most recent server snapshot. For my asteroids-a-like game I am predicting everything.

For each entity replicated from the server, clients will end up with two entities: one with a `Confirmed` component, which snaps to
the most recent authoritative update from the server, and one with either `Predicted` or `Interpolated`.

xxx



You define a Lightyear `Protocol`, which involves registering all the components you want to replicate.

```rust
// Score is server-authoritative and not predicted, just replicate any changes
// We don't predict/anticipate score changes on clients, we wait for the server to update scores.
app.register_component::<Score>(ChannelDirection::ServerToClient)
  .add_prediction(ComponentSyncMode::Simple);

// Linear Velocity should be fully predicted by clients. This will be update by the physics engine
// which is running on the client as well as the server.
app.register_component::<LinearVelocity>(ChannelDirection::ServerToClient)
  .add_prediction(ComponentSyncMode::Full);

// Position is fully predicted as part of the physics simulation too, but in addition we add:
// * correction_fn, which smears rollback errors due to misprediction over a few frames
// * interpolation_fn, which does visual interpolation for rendering positions between ticks
app.register_component::<Position>(ChannelDirection::ServerToClient)
  .add_prediction(ComponentSyncMode::Full)
  .add_interpolation_fn(position::lerp)
  .add_correction_fn(position::lerp);
```

Clients end up with two entities for each game object, one with a `Confirmed` component, which updates when server snapshots arrive, and one with a `Predicted` (or `Interpolated`) component. The predicted one is what you render on the clients, and we give them the same physics components as the entity on the server, so that we can simulate them ahead of most recent snapshot. When snapshots arrive, any misprediction errors are fixed by rollback, and blended in visually.

#### Blueprint pattern

I've found it especially useful to use the blueprint pattern for replication. For example, when a new player joins,
the server will spawn an entity for the player with an empty marker component `Player`, plus the essential replicated components like Position, LinearVelocity, etc.

Then another system shared between server and client will decorate newly added players with all the other stuff:

```rust
// When a newly added Player is found, insert all the useful player related components we need.
// This happens on client and server.
fn decorate_new_player_ships(
    q: Query<(Entity, &Player), (Added<Player>, RenderedEntity)>,
    mut commands: Commands,
) {
    for (e, player) in q.iter() {
        let player_hull = PlayerHull::default();
        let collider = player_hull.collider();

        commands.entity(e).insert((
            Name::new(format!("Player {}", player.nickname)),
            ColliderDensity(2.0),
            SpatialBundle::default(),
            player_hull.to_collider(),
            RigidBody::Dynamic,
            Hp::infinite(),
            // etc..
        ));
    }
}
```

Because that system runs on both server and clients, it's also filtering the query on `RenderedEntity`

```rust
    // A type for queries to filter entities that should be rendered.
    // On the client, this excludes Confirmed entities, and on the server,
    // it includes entities that are replicated to clients (in case the server runs a gui).
    cfg_if::cfg_if! {
        // important to test sandbox first, since it exists alongside client.
        if #[cfg(feature = "sandbox")] {
            pub type RenderedEntity = Without<lightyear::client::components::Confirmed>;
        } else if #[cfg(feature = "client")] {
            pub type RenderedEntity = With<Predicted>;
        } else if #[cfg(feature = "server")] {
            pub type RenderedEntity = With<ReplicationTarget>;
        } else {
            compile_error!("No RenderedEntity type for this build");
        }
    }
```



Additionally, on the clients, there is another system to decorate new players with the rendering components.